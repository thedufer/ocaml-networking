open! Core

type t

module Attr : sig
  type t = string * string
end

type tag = ?attrs:Attr.t list -> t list -> t

val tag : string -> tag

val text : string -> t

val html : tag
val head : tag
val body : tag
val div : tag
val img : tag
val script : tag
val link : tag

val src : string -> Attr.t
val alt : string -> Attr.t

val raw : string -> t

val to_string : t -> string
