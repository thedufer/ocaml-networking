open Core

type t = {
  port : int;
  bytes : char list;
} [@@deriving bin_io]
