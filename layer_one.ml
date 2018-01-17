open Core
open Async
open Sdn_local_protocol

let header = ['\xf0'; '\xcc'; '\xaa'; '\xab']
let header_bits = List.concat_map header ~f:Util.char_to_bools

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
  Bytes_.of_int ~num_bytes:4 (Int32.to_int_exn int)

let find_prefix q new_bits ~length ~f =
  let rec loop new_bits =
    if Int.equal (Queue.length q) length && f (Queue.to_list q) then begin
      let prefix = Queue.to_list q in
      Queue.filter_inplace q ~f:(const false);
      Some (`Remaining_bits new_bits, `Prefix prefix)
    end else begin
      if Int.equal (Queue.length q) length then
        ignore (Queue.dequeue q);
      match new_bits with
      | [] -> None
      | x :: xs ->
        Queue.enqueue q x;
        loop xs
    end
  in
  loop new_bits

module Reading = struct
  module State = struct
    type t =
      | Start
      | Found_header
      | Got_length of int
      | Got_data of {data : char list; crc : char list}
  end

  type t = {
    state : State.t;
    queue : bool Queue.t;
  }

  let start () = {
    state = Start;
    queue = Queue.create ();
  }

  let process_bits t bits =
    let rec loop t bits packets =
    match t.state with
    | Start -> begin
        let result =
          find_prefix t.queue bits ~length:(List.length header_bits)
            ~f:(List.equal ~equal:Bool.equal header_bits)
        in
        match result with
        | None -> (t, packets)
        | Some (`Remaining_bits bits, `Prefix _) -> loop {t with state = Found_header} bits packets
      end
    | Found_header -> begin
        let result = find_prefix t.queue bits ~length:16 ~f:(const true) in
        match result with
        | None -> (t, packets)
        | Some (`Remaining_bits bits, `Prefix len_bits) ->
          let len =
            let (lists, extra_bits) = Util.chunks len_bits 8 in
            if not (List.is_empty extra_bits) then
              raise_s [%message "bad len part" (len_bits : bool list) (lists : bool list list) (extra_bits : bool list)];
            List.map lists ~f:Util.bools_to_char
            |> Bytes_.to_int ~num_bytes:2
          in
          loop {t with state = Got_length len} bits packets
      end
    | Got_length len -> begin
        let result = find_prefix t.queue bits ~length:(len * 8) ~f:(const true) in
        match result with
        | None -> (t, packets)
        | Some (`Remaining_bits bits, `Prefix data) ->
          let (lists, extra_bits) = Util.chunks data 8 in
          assert (List.is_empty extra_bits);
          let data = List.map lists ~f:Util.bools_to_char in
          let crc = calc_crc data in
          loop {t with state = Got_data {data; crc}} bits packets
      end
    | Got_data {data; crc} -> begin
        let result = find_prefix t.queue bits ~length:32 ~f:(const true) in
        match result with
        | None -> (t, packets)
        | Some (`Remaining_bits bits, `Prefix crc') ->
          let (lists, extra_bits) = Util.chunks crc' 8 in
          assert (List.is_empty extra_bits);
          let crc' = List.map lists ~f:Util.bools_to_char in
          let packets =
            if List.equal ~equal:Char.equal crc crc' then
              data :: packets
            else
              packets
          in
          loop {t with state = Start} bits packets
      end
    in
    loop t bits []
end

let reader pipe =
  let (r, w) = Pipe.create () in
  let states = Int.Table.create () in
  Pipe.transfer pipe w ~f:(fun (msg : Message.t) ->
      let bits = List.concat_map msg.data ~f:Util.char_to_bools in
      let state =
        Hashtbl.find_or_add states msg.port ~default:(fun () -> Reading.start ())
      in
      let (state, result) = Reading.process_bits state bits in
      Hashtbl.update states msg.port (const state);
      List.map result ~f:(fun data -> {msg with data})
    )
  |> don't_wait_for;
  Pipe.map' r ~f:(fun q ->
      Queue.to_list q
      |> List.concat
      |> Queue.of_list
      |> Deferred.return)

let writer pipe =
  let (r, w) = Pipe.create () in
  Pipe.transfer r pipe ~f:(fun (msg : Message.t) ->
      let data =
        let length =
          let i = List.length msg.data in
          Bytes_.of_int i ~num_bytes:2
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
