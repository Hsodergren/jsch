type t

(* Creates an instance of t *)
val of_string: string -> (t,string) result

(*  *)
type validate_result = [ `Valid
                       | `Invalid of (Jsonpath.t * string) list]

(* [validate json schema] validate the a [json] document against the [schema] *)
val validate: Yojson.Safe.t -> t -> validate_result
