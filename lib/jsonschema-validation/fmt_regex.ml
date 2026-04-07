(** Format validator: regex (ECMA-262). *)

open Jsonschema_core

let check_regex s =
  match Regex.compile s with
  | Ok _ -> true
  | Error _ -> false
