open Core

let char_to_bools c =
  let c = Char.to_int c in
  List.init 8 ~f:(fun i ->
      ((1 lsl i) land c) > 0)

let bools_to_char bs =
  List.foldi bs ~init:0 ~f:(fun i c b ->
      (Bool.to_int b lsl i) lor c)
  |> Char.of_int_exn

let chunks l n =
  assert (n > 0);
  let (rev_chunks, rev_remaining_bits) =
    List.fold l ~init:([], [])
      ~f:(fun (rev_chunks, rev_remaining_elts) elt ->
          if List.length rev_remaining_elts = n - 1 then
            let new_chunk = List.rev (elt :: rev_remaining_elts) in
            (new_chunk :: rev_chunks, [])
          else
            (rev_chunks, elt :: rev_remaining_elts))
  in
  (List.rev rev_chunks, List.rev rev_remaining_bits)
