(** JSON Schema draft-07 validation. *)

open Jsonschema_core

type validator = Registry.validator
type error = Error.t

let compile ?(format_assertion = false) ?base_uri ?retriever schema_json =
  let reg = Registry.create ?retriever () in
  let base_uri = match base_uri with Some u -> u | None -> Uri.empty in
  Registry.add reg base_uri schema_json;
  let config : Compiler.config = {
    keywords = Draft07.all_keywords;
    format_assertion;
  } in
  Compiler.compile config reg ~base_uri schema_json

let validate v instance =
  v instance Json_pointer.empty

let is_valid v instance =
  validate v instance = []
