type t = { description: string option
         ; title: string option
         ; enum: Yojson.Safe.t list option
         ; value: value }
and value = | Number of numberv
            | Integer of numberv
            | String of stringv
            | Boolean
            | Object of objectv
            | Array of arrayv
and numberv = { max: float option
              ; min: float option }
and stringv = { str_max_length: int option
              ; str_min_length: int option }
and arrayv = { items: t
             ; arr_max_length: int option
             ; arr_min_length: int option }
and objectv = { properties: properties
              ; required: string list option }
and properties = | Props of (string * t) list
                 | PatProps of (Re.re * string * t) list

(* Creates an instance of t *)
val of_string: string -> (t,string) result

(*  *)
type validate_result = [ `Valid
                       | `Invalid of (Jsonpath.t * string) list]

(* [validate json schema] validate the a [json] document against the [schema] *)
val validate: Yojson.Safe.t -> t -> validate_result
