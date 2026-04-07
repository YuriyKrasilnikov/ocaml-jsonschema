(** URI utilities for JSON Schema.

    Handles base URI tracking, resolution of URI-references against
    base URIs, and fragment extraction — per RFC 3986 Section 5. *)

(** Resolve a URI-reference against a base URI.
    Per RFC 3986 Section 5.2. *)
val resolve : base:Uri.t -> Uri.t -> Uri.t

(** Normalize a URI: lowercase scheme/host, remove default port,
    remove dot segments. *)
val normalize : Uri.t -> Uri.t

(** Extract the fragment from a URI, if any. Returns [None] for
    URIs without a fragment. Returns [Some ""] for empty fragments. *)
val fragment : Uri.t -> string option

(** Return a URI with its fragment removed. *)
val without_fragment : Uri.t -> Uri.t
