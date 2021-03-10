open! Core

type t = char list

val of_int : int -> num_bytes:int -> char list
val to_int : char list -> num_bytes:int -> int

val of_int64 : int64 -> num_bytes:int -> char list
val to_int64 : char list -> num_bytes:int -> int64
