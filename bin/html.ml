open Core

module Attr = struct
  type t = string * string

  let to_string (name, value) = sprintf "%s=\"%s\"" name value
end

type t =
  | Tag of {name: string; attrs : Attr.t list; children: t list}
  | Text of string

type tag = ?attrs:Attr.t list -> t list -> t

let tag name ?(attrs=[]) children =
  Tag {name; attrs; children}

let text text = Text text

let html = tag "html"
let head = tag "head"
let body = tag "body"
let div  = tag "div"
let img = tag "img"

let src value = ("src", value)
let alt value = ("alt", value)

let rec to_string = function
  | Text text -> text
  | Tag {name; attrs; children} ->
    sprintf "<%s %s>%s</%s>"
      name
      (List.map attrs ~f:Attr.to_string |> String.concat ~sep:" ")
      (List.map children ~f:to_string |> String.concat)
      name
