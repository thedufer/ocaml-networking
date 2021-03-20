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

let switch r w ~ports ~expiration =
  let r = reader r in
  let w = writer w in
  let map = Address.Table.create () in
  let is_expired t =
    Time.is_earlier t ~than:(Time.sub (Time.now ()) expiration)
  in
  Clock.every expiration (fun () ->
      Hashtbl.filter_inplace map ~f:(fun (_, t) ->
          not (is_expired t)));
  let r =
    Pipe.map' r ~f:(fun q ->
        List.concat_map (Queue.to_list q) ~f:(fun msg ->
            Hashtbl.set map ~key:msg.from ~data:(msg.msg.port, Time.now ());
            match Hashtbl.find map msg.to_ with
            | Some (port, t) when not (is_expired t) ->
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
    | Forwarding of [`Root | `Designated | `Blocked] Int.Map.t

  type t =
    { mutable root_id : Address.t
    ; mutable root_distance : int
    ; mutable root_direction : int option
    ; mutable state : state
    }

  let address = Address.of_int64 0x01_00_00_00_00_00_00_00L

  let listening_length = Time.Span.of_sec 15.

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

  let handle_message t (msg : Sdn_local_protocol.Message.t) =
    let parsed = Message.of_bytes msg.data in
    if Address.(parsed.root_id < t.root_id) then
      (t.root_id <- parsed.root_id;
       t.root_distance <- parsed.root_distance + 1;
       t.root_direction <- Some msg.port;
       t.state <-
         match t.state with
         | Listening _ | Forwarding _ ->
           Listening { until = Time.add (Time.now ()) listening_length }
      );
    if Address.equal parsed.root_id t.root_id &&
       parsed.root_distance + 1 < t.root_distance then
      (t.root_distance <- parsed.root_distance + 1;
       t.root_direction <- Some msg.port)
end

let stp_switch r w ~ports ~me =
  let r = reader r in
  let w = writer w in
  let t =
    { Stp.root_id = Address.create me 0xffff
    ; root_distance= 0
    ; root_direction=None
    ; state = Listening { until = Time.add (Time.now ()) Stp.listening_length }
    }
  in
  let write = Pipe.write_without_pushback w in
  let send_control_packet () =
    List.init ports ~f:Fn.id
    |> List.iter ~f:(fun port ->
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
  Clock.every (Time.Span.of_sec 2.) send_control_packet;
  Pipe.iter r ~f:(fun msg ->
      if Address.equal msg.to_ Stp.address then
        (Stp.handle_message t msg.msg;
         return ())
      else
        return ())
