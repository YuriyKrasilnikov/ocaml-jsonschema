(** JSON instance data model for JSON Schema.

    Provides equality, type checking, and utility functions that match
    the JSON Schema specification semantics (not OCaml structural equality). *)

type json = Yojson.Safe.t

(** Deep equality per JSON Schema spec.
    Numbers compare by mathematical value: [1] = [1.0].
    Booleans are never equal to numbers: [true] <> [1].
    Objects compare as unordered key-value sets. *)
val json_equal : json -> json -> bool

(** Count Unicode codepoints in a UTF-8 string.
    This is the "length" used by maxLength/minLength per spec. *)
val utf8_length : string -> int

(** Convert a JSON number to float. Returns [None] for non-numbers. *)
val to_float : json -> float option

(** Convert a JSON number to int if possible. Returns [None] for non-integers
    and non-numbers. [Float 1.0] returns [Some 1]. *)
val to_int : json -> int option

(** Check if a JSON value matches a JSON Schema type name.
    Type names: "null", "boolean", "object", "array", "number", "string", "integer".
    "integer" matches any number with zero fractional part. *)
val is_type : string -> json -> bool
