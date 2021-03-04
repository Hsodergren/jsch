type t [@@deriving eq]

type fragment = [ `Object of string
                | `Index of int ]

val empty: t

val compare: t -> t -> int

val add: t -> fragment -> t

val to_string: t -> string

val to_list: t -> fragment list
