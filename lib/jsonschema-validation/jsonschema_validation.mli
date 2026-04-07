(** JSON Schema draft-07 validation.

    Compiles JSON Schema documents into validators and validates
    JSON instances against them. *)

(** Re-export core types. *)
type validator = Jsonschema_core.Registry.validator
type error = Jsonschema_core.Error.t

(** Compile a JSON Schema into a validator.
    @param format_assertion if true, "format" keyword validates (default: false)
    @param base_uri base URI for $ref resolution
    @param retriever function to load external schemas by URI *)
val compile :
  ?format_assertion:bool ->
  ?base_uri:Uri.t ->
  ?retriever:(Uri.t -> Yojson.Safe.t option) ->
  Yojson.Safe.t -> validator

(** Validate a JSON instance against a compiled validator. *)
val validate : validator -> Yojson.Safe.t -> error list

(** Check if a JSON instance is valid against a compiled validator. *)
val is_valid : validator -> Yojson.Safe.t -> bool
