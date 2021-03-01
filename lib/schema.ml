open Printf

type validate_result = [ `Valid
                       | `Invalid of (Jsonpath.t * string) list]

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

let kind = function
  | Number _ -> "number"
  | Integer _ -> "integer"
  | String _ -> "string"
  | Boolean -> "bool"
  | Array _ -> "array"
  | Object _ -> "object"

let json_opt v =
  match v with
  | `Null -> None
  | v -> Some v

let get_path full_path json =
  let rec aux path json =
    match path with
    | [] -> json
    | hd::tl ->
      match Yojson.Safe.Util.member hd json with
      | `Null -> failwith "Doesn't exist"
      | v -> aux tl v
  in
  aux full_path json

let parse_path str =
  let len = String.length str in
  String.sub str 2 (len - 2)
  |> String.split_on_char '/'

let rec list_result_map ~f l =
  let (>>=) = Result.bind in
  match l with
  | [] -> Ok []
  | hd::tl ->
    f hd >>= fun hd ->
    list_result_map ~f tl >>= fun tl ->
    Ok (hd::tl)

let to_type_option_r f j =
  Yojson.Safe.Util.(try Ok (f j) with Type_error (s,_) -> Error s)
let to_int_option_r j = to_type_option_r Yojson.Safe.Util.to_int_option j
let to_number_option_r j = to_type_option_r Yojson.Safe.Util.to_number_option j
let to_string_option_r j = to_type_option_r Yojson.Safe.Util.to_string_option j

let to_list_option msg = function
  | `Null -> Ok None
  | `List l -> Ok (Some l)
  | _ -> Error (msg ^ " doesn't contain a list")

let compile_rexexp s = Re.Posix.(compile (re s))


let of_yojson full_json =
  let open Yojson.Safe in
  let (let*) = Result.bind in
  let (>>=) = Result.bind in
  let (>>|) m f = Result.map f m in
  let rec follow_ref json =
    match Util.member "$ref" json with
    | `String s -> follow_ref @@ get_path (parse_path s) full_json
    | `Null -> Ok json
    | _ -> Error "$ref field must be a string field"
  in
  let parse_string json =
    let* str_max_length = Util.member "maxLength" json |> to_int_option_r in
    let* str_min_length = Util.member "minLength" json |> to_int_option_r in
    Ok (String {str_max_length;str_min_length})
  in
  let parse_number json =
    let* min = Util.member "minimum" json |> to_number_option_r in
    let* max = Util.member "maximum" json |> to_number_option_r in
    Ok ({min;max})
  in
  let rec parse json =
    let* json = follow_ref json in
    let* description = Util.member "description" json |> to_string_option_r in
    let* title = Util.member "title" json |> to_string_option_r in
    let* enum = Util.member "enum" json |> to_list_option "enum" in
    let* value = match Util.member "type" json with
      | `String "integer" -> parse_number json >>| fun num -> Integer num
      | `String "number" -> parse_number json >>| fun num -> Number num
      | `String "string" -> parse_string json
      | `String "boolean" -> Ok Boolean
      | `String "object" -> parse_object json
      | `String "array" -> parse_array json
      | `String s -> Error ("invalid type: " ^ s)
      | `Null -> Error "A type definition must have a 'type' field"
      | _ -> Error "'type' field must be string"
    in
    Ok ({description; title; enum; value})
  and parse_object json =
    let mem str = Util.member str json in
    let to_prop assoc =
      list_result_map ~f:(fun (s, j) -> parse j >>| fun j -> s,j) assoc >>= fun l ->
      Ok (Props l)
    in
    let to_pat_prop assoc =
      list_result_map ~f:(fun (s, j) -> parse j >>| fun j ->  compile_rexexp s,s,j) assoc >>= fun l ->
      Ok (PatProps l)
    in
    let* propf, propjson =
      match mem "properties",  mem "patternProperties" with
     | `Null, `Null -> Error "need properties or patterProperties"
     | `Null, json -> Ok (to_pat_prop, json)
     | json, _ -> Ok (to_prop, json)
    in
    let* properties = propjson
                      |> Util.to_assoc
                      |> propf
    in
    let required =
      let r = Util.member "required" json in
      Option.map (fun j -> j |> Util.to_list |> Util.filter_string) (json_opt r)
    in
    Ok (Object {properties; required})
  and parse_array json =
    let* items = Util.member "items" json |> parse in
    let* arr_max_length = Util.member "maxItems" json |> to_int_option_r in
    let* arr_min_length = Util.member "minItems" json |> to_int_option_r in
    Ok (Array {items; arr_min_length; arr_max_length})
  in
  parse full_json

let of_string str =
  of_yojson @@ Yojson.Safe.from_string str

module Validate_Result = struct
  type t = | Valid
           | Invalid of (Jsonpath.t * string) list

  let empty = Valid
  let ok = Valid

  let error path str = Invalid [path,str]

  let merge t1 t2 =
    match t1,t2 with
    | Valid, Valid -> Valid
    | Invalid _ as e, Valid | Valid, (Invalid _ as e) -> e
    | Invalid e1, Invalid e2 -> Invalid (e1 @ e2)

  let combine l = List.fold_left merge empty l

  module Infix = struct
    let (<+>) = merge
  end
end

module VR = Validate_Result

let rec mem ~cmp v l =
  match l with
  | [] -> false
  | hd::tl -> if cmp v hd then true else mem ~cmp v tl


let find_first_matching str obj_strs =
  let find_it pred l =
    List.find_map (fun (s,j) -> if pred s then Some j else None) l
  in
  match obj_strs with
  | Props l -> find_it (String.equal str) l
  | PatProps l ->
    List.map (fun (a,_,b) -> a,b) l
    |> find_it (fun p -> Re.execp p str)

let validate json t =
  let open VR.Infix in
  let res_fold_opt ?(none=VR.ok) o f path message =
    match o with
    | None -> none
    | Some v -> if f v then VR.ok else VR.error path message
  in
  let validate_opt ?(none=VR.ok) o f = Option.fold ~none ~some:f o in
  let validate_enum path json enum =
    if mem ~cmp:Yojson.Safe.equal json enum
    then VR.ok
    else VR.error path (sprintf "value %s not in enum" (Yojson.Safe.to_string json))
  in
  let validate_required path assoc required =
    let fields = List.map fst assoc in
    let missing_fields = List.filter_map
        (fun req ->
           if List.mem req fields
           then None
           else Some (VR.error (Jsonpath.add path @@ `Object req) "required field missing"))
        required in
    VR.combine missing_fields
  in
  let validate_number path v {min;max} =
    let r1 = res_fold_opt min (fun min -> v >= min) path (sprintf "%f to small" v) in
    let r2 = res_fold_opt max (fun max -> v <= max) path (sprintf "%f to big" v) in
    r1 <+> r2
  in
  let rec validate_path path json {value;enum; _} =
    let validate_enum () =
      validate_opt enum @@ validate_enum path json
    in
    match json, value with
    | `Assoc obj, Object {properties; required} ->
      validate_object path obj properties
      <+>
      validate_opt required @@ validate_required path obj
    | `List l, Array {items; arr_max_length; arr_min_length} ->
      validate_list path l items
      <+>
      res_fold_opt arr_min_length (fun v -> List.length l >= v) path "List too short"
      <+>
      res_fold_opt arr_max_length (fun v -> List.length l <= v) path "List too long"
    | `Int i , Integer numberv ->
      validate_number path (float i) numberv <+> validate_enum ()
    | (`Int _ | `Float _), Number numberv ->
      let v = Yojson.Safe.Util.to_number json in
      validate_number path v numberv <+> validate_enum ()
    | `String s, String {str_max_length;str_min_length} ->
      let min_len = res_fold_opt str_min_length (fun v -> String.length s >= v) path "string too short" in
      let max_len = res_fold_opt str_max_length (fun v -> String.length s <= v) path "string too long" in
      max_len <+> min_len <+> validate_enum ()
    | `Bool _, Boolean -> VR.ok
    | _ -> VR.error path "wrong object type"
  and validate_object path assoc props =
    List.map
      (fun (name, js) ->
         let p = Jsonpath.add path @@ `Object name in
         match find_first_matching name props with
         | Some schema -> validate_path p js schema
         | None -> VR.error p "value doesn't exist in schema")
      assoc
    |> VR.combine
  and validate_list path l schema =
    List.mapi
      (fun i value -> validate_path (Jsonpath.add path (`Index i)) value schema) l
    |> VR.combine
  in
  match validate_path Jsonpath.empty json t with
  | Valid -> `Valid
  | Invalid l -> `Invalid l
