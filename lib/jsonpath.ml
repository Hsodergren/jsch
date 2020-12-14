type fragment = [ `Object of string
                | `Index of int ]

type t = fragment list

let empty = []

let add t f = f::t

let to_string t =
  let rec aux t =
    match t with
    | [`Index i] -> "[" ^ string_of_int i ^ "]"
    | [`Object s] -> s
    | `Object s::tl -> s ^ "." ^ aux tl
    | `Index i::tl -> "[" ^ string_of_int i ^ "]." ^ aux tl
    | [] -> ""
  in
  aux (List.rev t)

let to_list = List.rev
