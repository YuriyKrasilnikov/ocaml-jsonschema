(** Schema compiler — transforms JSON Schema into a validator function.

    The compiler dispatches to registered keyword compilers and handles
    boolean schemas, $ref resolution, and unknown keyword skipping.

    Keyword compilers are provided by vocabulary packages (e.g.,
    jsonschema-validation) and registered before compilation. *)

(** Configuration for compilation. *)
type config = {
  keywords : keyword_compiler list;
  format_assertion : bool;
}

(** A keyword compiler: given context and the keyword's value,
    returns a validator or None if the keyword is not applicable. *)
and keyword_compiler = {
  keyword : string;
  compile : context -> Yojson.Safe.t -> Registry.validator option;
}

(** Context passed to keyword compilers. *)
and context = {
  config : config;
  registry : Registry.t;
  base_uri : Uri.t;
  schema_path : Json_pointer.t;
  schema_obj : Yojson.Safe.t;
  compile_sub : Yojson.Safe.t -> Registry.validator;
}

(** Default config with no keywords. *)
val default_config : config

(** Compile a JSON Schema into a validator.

    The schema is compiled against the given registry, using the
    provided keyword compilers. Boolean schemas are handled directly.
    $ref is handled before keyword dispatch: when $ref is present,
    all other keywords in the object are ignored. *)
val compile : config -> Registry.t -> ?base_uri:Uri.t -> Yojson.Safe.t -> Registry.validator
