(** JSON Pointer (RFC 6901).

    A JSON Pointer is a string syntax for identifying a specific value
    within a JSON document. Example: [/foo/bar/0] points to the first
    element of the array at key "bar" inside object "foo". *)

(** A parsed JSON Pointer as a list of unescaped reference tokens.
    The empty list represents the root pointer [""]. *)
type t = string list

(** Parse a JSON Pointer string into reference tokens.
    Returns [Error] for strings that don't start with [/] (except empty string).
    Unescapes [~1] to [/] and [~0] to [~]. *)
val of_string : string -> (t, string) result

(** Serialize a JSON Pointer to its string representation.
    Escapes [~] to [~0] and [/] to [~1]. *)
val to_string : t -> string

(** Resolve a JSON Pointer against a JSON value.
    Returns the sub-value at the given path, or [None] if the path
    does not exist in the document. *)
val resolve : t -> Yojson.Safe.t -> Yojson.Safe.t option

(** Append a single reference token to a pointer. *)
val append : t -> string -> t

(** The empty (root) pointer. *)
val empty : t
