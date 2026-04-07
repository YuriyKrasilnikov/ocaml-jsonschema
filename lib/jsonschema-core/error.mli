(** Validation error types.

    Errors carry both the instance path (where in the data) and the
    schema path (which keyword). Combinator keywords (allOf, anyOf, etc.)
    attach sub-errors in [context]. *)

(** A validation error. *)
type t = {
  instance_path : Json_pointer.t;
  schema_path : Json_pointer.t;
  keyword : string;
  message : string;
  context : t list;
}

(** Create an error at the given paths. *)
val make :
  instance_path:Json_pointer.t ->
  schema_path:Json_pointer.t ->
  keyword:string ->
  string -> t

(** Create an error with sub-errors (for combinators like allOf, anyOf). *)
val make_with_context :
  instance_path:Json_pointer.t ->
  schema_path:Json_pointer.t ->
  keyword:string ->
  context:t list ->
  string -> t

(** Format error as human-readable string (for debugging). *)
val to_string : t -> string
