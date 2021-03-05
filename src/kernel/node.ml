open! Core_kernel

module Id = struct
  type t = string [@@deriving bin_io, compare, sexp]

  let of_string x = x
  let to_string x = x

  let equal = String.equal
end

type t = {
  id : Id.t;
  ports : int;
  address : Address.t;
} [@@deriving bin_io, sexp]

let has_port t port =
  port >= 0 && port < t.ports
