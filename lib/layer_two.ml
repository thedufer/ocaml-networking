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
