(** Validation error types. *)

type t = {
  instance_path : Json_pointer.t;
  schema_path : Json_pointer.t;
  keyword : string;
  message : string;
  context : t list;
}

let make ~instance_path ~schema_path ~keyword message =
  { instance_path; schema_path; keyword; message; context = [] }

let make_with_context ~instance_path ~schema_path ~keyword ~context message =
  { instance_path; schema_path; keyword; message; context }

let to_string e =
  Printf.sprintf "%s: %s (keyword: %s, schema: %s)"
    (Json_pointer.to_string e.instance_path)
    e.message
    e.keyword
    (Json_pointer.to_string e.schema_path)
