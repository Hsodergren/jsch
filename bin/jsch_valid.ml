open Cmdliner
open Jsch

let read_file f =
  let ch = open_in f in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let format_error (path,str) =
  let path = Jsonpath.to_string path in
  Printf.sprintf "%s : %s" path str

let rec intersperse ~sep = function
  | [] -> ""
  | [hd] -> hd
  | hd::tl -> hd ^ sep ^ intersperse ~sep tl

let ( let* ) = Result.bind
let validate file schema =
  let aux =
    let json = Yojson.Safe.from_file file in
    let* schema = Schema.of_string (read_file schema) in
    match Schema.validate json schema with
    | `Valid -> Ok ()
    | `Invalid errs ->
      let errs = List.map format_error errs in
      Error (intersperse ~sep:"\n" errs)
  in
  match aux with
  | Ok () -> ()
  | Error s -> Printf.eprintf "%s\n" s

let schema =
  let doc = "Schema file to validate against" in
  Arg.(required & opt (some file) None & info ["s"; "schema"] ~docv:"SCHEMA" ~doc)

let file =
  let doc = "The json file to validate" in
  Arg.(required & opt (some file) None & info ["f"; "file"] ~docv:"JSON_FILE" ~doc)

let cmd =
  let doc = "JSON Schema validator" in
  let man = [`P "Validate a json file against a json schema"] in
  Term.info "jsch_valid" ~version:"%%VERSION%%" ~doc ~man

let validate_t = Term.(const validate $ file $ schema)
let () = Term.exit @@ Term.eval (validate_t, cmd)
