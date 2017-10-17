open Core
open Async
open Sdn_local_protocol

(* TODO *)
let reader pipe = pipe

let header = ['\xf0'; '\xcc'; '\xaa'; '\xab']

let calc_crc bytes =
  let poly = Int32.of_int_exn 0x04C11DB7 in
  let int =
    List.fold bytes ~init:Int32.minus_one ~f:(fun crc byte ->
        let crc = ref crc in
        let byte_int =
          Util.char_to_bools byte
          |> List.foldi ~init:Int32.zero ~f:(fun i acc bit ->
              if bit then
                Int32.(bit_or acc (shift_left one Int.(31 - i)))
              else
                acc
            )
        in
        List.iter (List.init 8 ~f:Fn.id) ~f:(fun i ->
            let shifted_crc = Int32.(lsl) !crc 1 in
            if Int32.(bit_xor (!crc) (shift_left byte_int i) < zero) then
              crc := Int32.bit_xor shifted_crc poly
            else
              crc := shifted_crc
          );
        !crc
      )
  in
  let int = Int32.bit_not int in
  printf !"%{Int32}\n" int;
  [ Int32.(bit_and (of_int_exn 0xff) (int lsr 24)) |> Int32.to_int_exn |> Char.of_int_exn
  ; Int32.(bit_and (of_int_exn 0xff) (int lsr 16)) |> Int32.to_int_exn |> Char.of_int_exn
  ; Int32.(bit_and (of_int_exn 0xff) (int lsr 8))  |> Int32.to_int_exn |> Char.of_int_exn
  ; Int32.(bit_and (of_int_exn 0xff)  int)         |> Int32.to_int_exn |> Char.of_int_exn
  ]
  |> List.rev
  |> List.map ~f:Util.char_to_bools
  |> List.map ~f:List.rev
  |> List.map ~f:Util.bools_to_char

let writer pipe =
  let (r, w) = Pipe.create () in
  Pipe.transfer r pipe ~f:(fun (msg : Message.t) ->
      let data =
        let length =
          let i = List.length msg.data in
          assert (i < Int.pow 2 16);
          [(i lsr 8) land 0xff |> Char.of_int_exn; i land 0xff |> Char.of_int_exn]
        in
        List.concat [
          header;
          length;
          msg.data;
          calc_crc msg.data;
        ]
      in
      {msg with data})
  |> don't_wait_for;
  w
