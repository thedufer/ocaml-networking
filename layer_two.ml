open Core
open Async
open Sdn_local_protocol

type t = {
  msg : Message.t;
  to_ : Address.t;
  from : Address.t;
}

let reader pipe =
  let (r, w) = Pipe.create () in
  Pipe.transfer (Layer_one.reader pipe) w ~f:(fun (msg : Message.t) ->
      let (to_bytes,   data) = List.split_n msg.data 8 in
      let (from_bytes, data) = List.split_n data     8 in
      let to_  = Bytes_.to_int to_bytes   ~num_bytes:8 |> Address.of_int in
      let from = Bytes_.to_int from_bytes ~num_bytes:8 |> Address.of_int in
      let msg = {msg with data} in
      {msg; to_; from})
  |> don't_wait_for;
  r

let writer pipe =
  let (r, w) = Pipe.create () in
  Pipe.transfer r (Layer_one.writer pipe) ~f:(fun {msg; to_; from} ->
      let data =
        List.concat [
          Bytes_.of_int (Address.to_int to_)  ~num_bytes:8;
          Bytes_.of_int (Address.to_int from) ~num_bytes:8;
          msg.data;
        ]
      in
      {msg with data})
  |> don't_wait_for;
  w
