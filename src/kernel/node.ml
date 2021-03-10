open! Core_kernel

module Id = struct
  module T = struct
    type t = string [@@deriving bin_io, compare]

    let char_allowed = function
      | 'a'..'z'
      | 'A'..'Z'
      | '0'..'9'
      | '_' -> true
      | _ -> false

    let of_string x =
      if not (String.for_all x ~f:char_allowed) then
        raise_s [%message "node ids can only contain letters, numbers, and underscores"];
      x
    let to_string x = x
  end

  include T
  include Sexpable.Of_stringable (T)

  let equal = String.equal
end

type t = {
  id : Id.t;
  ports : int;
  address : Address.Node.t;
} [@@deriving bin_io, sexp]

let has_port t port =
  port >= 0 && port < t.ports
