(** Schema registry — stores and resolves schemas by URI.

    Handles $id registration, $ref resolution, base URI tracking,
    and circular reference detection via the placeholder pattern. *)

(** The registry. Mutable during schema indexing/compilation. *)
type t

(** A compiled validator: instance -> instance_path -> errors. *)
type validator = Yojson.Safe.t -> Json_pointer.t -> Error.t list

(** Optional retriever for loading external schemas by URI. *)
type retriever = Uri.t -> Yojson.Safe.t option

(** Create an empty registry with an optional retriever for remote schemas. *)
val create : ?retriever:retriever -> unit -> t

(** Add a raw schema to the registry at the given URI.
    Also scans the schema for $id keywords and registers sub-schemas. *)
val add : t -> Uri.t -> Yojson.Safe.t -> unit

(** Resolve a URI to its raw JSON schema. Tries the registry first,
    then the retriever if one was provided. Returns [None] if not found. *)
val resolve_raw : t -> Uri.t -> Yojson.Safe.t option

(** Resolve a URI and return both the schema and its effective base URI
    (accounting for $id keywords along the JSON Pointer path). *)
val resolve_with_base : t -> Uri.t -> (Yojson.Safe.t * Uri.t) option

(** Store a compiled validator at the given URI. *)
val set_compiled : t -> Uri.t -> validator -> unit

(** Get a compiled validator, if available. *)
val get_compiled : t -> Uri.t -> validator option

(** Check if a URI is currently being compiled (cycle detection). *)
val is_compiling : t -> Uri.t -> bool

(** Mark a URI as currently being compiled. *)
val mark_compiling : t -> Uri.t -> unit

(** Register a callback to be called when a cyclic ref's compilation
    completes. The callback receives the final validator. *)
val on_compiled : t -> Uri.t -> (validator -> unit) -> unit
