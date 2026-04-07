(** Regex engine abstraction.

    Wraps the regex engine behind a module boundary so it can be
    swapped (Re.Perl now, pcre2/oniguruma later if needed).
    JSON Schema requires ECMA-262 regex semantics. *)

(** A compiled regular expression. *)
type t

(** Compile a regex pattern string. Returns [Error] if the pattern
    is invalid. JSON Schema regexes are NOT implicitly anchored. *)
val compile : string -> (t, string) result

(** Test whether the regex matches anywhere in the string.
    Per JSON Schema spec, patterns are NOT anchored. *)
val matches : t -> string -> bool
