(** JSON Schema representation.

    A JSON Schema "MUST be an object or a boolean" (spec §4.3.1).
    Boolean schemas: [true] always validates, [false] never validates. *)

(** A JSON Schema. *)
type t =
  | Bool of bool
  | Obj of Yojson.Safe.t  (** The raw schema object for keyword access. *)

(** Parse a JSON value as a schema. Returns [Error] if the value
    is neither an object nor a boolean. *)
val of_json : Yojson.Safe.t -> (t, string) result

(** Get a keyword value from a schema object. Returns [None] for
    boolean schemas or if the keyword is absent. *)
val keyword : string -> t -> Yojson.Safe.t option

(** Get all keyword names present in a schema object.
    Returns [[]] for boolean schemas. *)
val keywords : t -> string list
