open Core_kernel

include Unique_id.Int63 ()

(* These can't raise when [int] is [int63], which I expect to be the case. *)
let of_int = of_int_exn
let to_int = to_int_exn

let arg_type = Command.Arg_type.map (Command.Param.int) ~f:of_int
