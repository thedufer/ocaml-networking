open! Core_kernel

val bools_to_char : bool list -> char
val char_to_bools : char -> bool list

val chunks : 'a list -> int -> 'a list list * 'a list
