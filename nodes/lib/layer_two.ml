open Core
open Async
open Sdn_local_protocol

type t = {
  msg : Message.t;
  to_ : Address.t;
  from : Address.t;
} [@@deriving sexp_of]

let reader pipe =
  let (r, w) = Pipe.create () in
  Pipe.transfer (Layer_one.reader pipe) w ~f:(fun (msg : Message.t) ->
      let (to_bytes,   data) = List.split_n msg.data 8 in
      let (from_bytes, data) = List.split_n data     8 in
      let to_  = Bytes_.to_int64 to_bytes   ~num_bytes:8 |> Address.of_int64 in
      let from = Bytes_.to_int64 from_bytes ~num_bytes:8 |> Address.of_int64 in
      let msg = {msg with data} in
      let t = {msg; to_; from} in
      t)
  |> don't_wait_for;
  r

let writer pipe =
  let (r, w) = Pipe.create () in
  Pipe.transfer r (Layer_one.writer pipe) ~f:(fun {msg; to_; from} ->
      let data =
        List.concat [
          Bytes_.of_int64 (Address.to_int64 to_)  ~num_bytes:8;
          Bytes_.of_int64 (Address.to_int64 from) ~num_bytes:8;
          msg.data;
        ]
      in
      {msg with data})
  |> don't_wait_for;
  w

let hub r w ~ports =
  let r = reader r in
  let w = writer w in
  let r =
    Pipe.map' r ~f:(fun q ->
        List.concat_map (Queue.to_list q) ~f:(fun msg ->
            List.init ports ~f:(fun port ->
                if Int.equal msg.msg.port port then
                  None
                else
                  Some {msg with msg = {msg.msg with port}})
            |> List.filter_opt)
        |> Queue.of_list
        |> Deferred.return)
  in
  Pipe.transfer_id r w

let is_expired ~expiration time =
  Time.is_earlier time ~than:(Time.sub (Time.now ()) expiration)

let switch r w ~ports ~expiration =
  let r = reader r in
  let w = writer w in
  let map = Address.Table.create () in
  Clock.every expiration (fun () ->
      Hashtbl.filter_inplace map ~f:(fun (_, t) ->
          not (is_expired ~expiration t)));
  let r =
    Pipe.map' r ~f:(fun q ->
        List.concat_map (Queue.to_list q) ~f:(fun msg ->
            Hashtbl.set map ~key:msg.from ~data:(msg.msg.port, Time.now ());
            match Hashtbl.find map msg.to_ with
            | Some (port, t) when not (is_expired ~expiration t) ->
              [{msg with msg = {msg.msg with port = port}}]
            | Some _ | None ->
              List.init ports ~f:(fun port ->
                  if Int.equal msg.msg.port port then
                    None
                  else
                    Some {msg with msg = {msg.msg with port}})
              |> List.filter_opt)
        |> Queue.of_list
        |> Deferred.return)
  in
  Pipe.transfer_id r w

module Stp = struct
  type state =
    | Listening of { until : Time.t }
    | Forwarding

  type t =
    { me : Address.t
    ; mutable root_id : Address.t
    ; mutable root_distance : int
    ; mutable root_port : int option
    ; mutable state : state
    ; has_messaged_this_root : (Time.t * int) Int.Table.t
    ; routing : (int * Time.t) Address.Table.t
    }

  let address = Address.of_int64 0x01_00_00_00_00_00_00_00L

  let listening_length = Time.Span.of_sec 15.
  let message_timeout = Time.Span.of_sec 15.
  let expiration = Time.Span.of_sec 15.

  module Message = struct
    type t =
      { root_id : Address.t
      ; root_distance : int
      }

    let to_bytes {root_id; root_distance} =
      List.concat
        [ Address.to_int64 root_id |> Bytes_.of_int64 ~num_bytes:8
        ; Bytes_.of_int root_distance ~num_bytes:2
        ]

    let of_bytes bytes =
      let root_id, rest = List.split_n bytes 8 in
      let root_distance, rest = List.split_n rest 2 in
      assert (List.is_empty rest);
      { root_id = Bytes_.to_int64 root_id ~num_bytes:8 |> Address.of_int64
      ; root_distance = Bytes_.to_int root_distance ~num_bytes:2
      }
  end

  let new_root t id distance port =
    t.root_id <- id;
    t.root_distance <- distance;
    t.root_port <- port;
    t.state <- Listening { until = Time.add (Time.now ()) listening_length };
    Hashtbl.clear t.has_messaged_this_root

  let ping t =
    let should_send_message = ref false in
    let now = Time.now () in
    Hashtbl.filter_inplace t.routing ~f:(fun (_, t) ->
        not (is_expired ~expiration t));
    t.state <-
      (match t.state with
       | Forwarding -> Forwarding
       | Listening { until } when Time.is_earlier until ~than:now ->
         print_endline "transitioning to forwarding state";
         Forwarding
       | Listening _ as s -> s);
    Hashtbl.filter_inplace t.has_messaged_this_root ~f:(fun (time, _) ->
        Time.is_later (Time.add time message_timeout) ~than:now);
    (match t.root_port with
     | None -> ()
     | Some root_direction ->
       if not (Hashtbl.mem t.has_messaged_this_root root_direction) then
         (should_send_message := true;
          (* our path to the root is dead; reconfigure *)
          match
            Hashtbl.to_alist t.has_messaged_this_root
            |> List.min_elt ~compare:(Comparable.lift Int.compare ~f:fst)
          with
          | Some (direction, (_time, root_distance)) ->
            print_endline "picking new path to root";
            (* this is our new best path *)
            t.root_distance <- root_distance;
            t.root_port <- Some direction
          | None ->
            print_endline "assigning ourself root";
            (* no known path to root; change root to us and go through a new
               listening phase *)
            new_root t t.me 0 None));
    !should_send_message

  let handle_message t (msg : Sdn_local_protocol.Message.t) =
    let should_send_message = ref (ping t) in
    let parsed = Message.of_bytes msg.data in
    if Address.(parsed.root_id < t.root_id) then
      (print_s [%message "promoting root"
           ~from:(t.root_id : Address.t)
           ~to_:(parsed.root_id : Address.t)];
       should_send_message := true;
       new_root t parsed.root_id (parsed.root_distance + 1) (Some msg.port));
    if Address.equal parsed.root_id t.root_id &&
       parsed.root_distance + 1 < t.root_distance then
      (print_s [%message "found new best path"
           ~from:(t.root_port : int option)
           ~to_:(msg.port : int)];
        should_send_message := true;
       t.root_distance <- parsed.root_distance + 1;
       t.root_port <- Some msg.port);
    if Address.equal parsed.root_id t.root_id then
      Hashtbl.set t.has_messaged_this_root ~key:msg.port ~data:(Time.now (), parsed.root_distance + 1);
    !should_send_message
end

let stp_switch r w ~ports ~me =
  let r = reader r in
  let w = writer w in
  let me_full_addr = Address.create me 0xffff in
  let t =
    { Stp.me = me_full_addr
    ; root_id = me_full_addr
    ; root_distance = 0
    ; root_port = None
    ; state = Listening { until = Time.add (Time.now ()) Stp.listening_length }
    ; has_messaged_this_root = Int.Table.create ()
    ; routing = Address.Table.create ()
    }
  in
  let write = Pipe.write_without_pushback w in
  let send_control_packet () =
    List.init ports ~f:Fn.id
    |> List.iter ~f:(fun port ->
        (* Don't send these back towards the current root *)
        if not ([%equal: int option] (Some port) t.root_port) then
          let data =
            Stp.Message.to_bytes
              { root_id = t.root_id
              ; root_distance = t.root_distance
              }
          in
          write
            { to_ = Stp.address
            ; from = Address.create me port
            ; msg = {port;data}
            })
  in
  Clock.every (Time.Span.of_sec 2.) (fun () ->
      ignore (Stp.ping t : bool);
      send_control_packet ());
  Pipe.iter r ~f:(fun msg ->
      if Address.equal msg.to_ Stp.address then
        (if Stp.handle_message t msg.msg then send_control_packet ())
      else
        (if Stp.ping t then send_control_packet ();
         Hashtbl.set t.routing ~key:msg.from ~data:(msg.msg.port, Time.now ());
         match t.state with
         | Listening _ -> ()
         | Forwarding ->
           match Hashtbl.find t.routing msg.to_ with
           | Some (port, t) when not (is_expired ~expiration:Stp.expiration t) ->
             write {msg with msg = {msg.msg with port = port}}
           | Some _ | None ->
             List.init ports ~f:(fun port ->
                 if Int.equal msg.msg.port port then
                   (* don't send it where it came from *)
                   None
                 else if Hashtbl.mem t.has_messaged_this_root port &&
                         not ([%equal: int option] t.root_port (Some port)) then
                   None
                 else
                   Some {msg with msg = {msg.msg with port}})
             |> List.filter_opt
             |> List.iter ~f:write);
      return ())
