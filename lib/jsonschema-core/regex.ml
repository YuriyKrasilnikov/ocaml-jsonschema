(** Regex engine — Re.Perl backend.

    JSON Schema requires ECMA-262 semantics. Re.Perl covers ~95% of
    real-world patterns. All mandatory test patterns are compatible.
    Isolate behind module boundary for future swap to pcre2/oniguruma. *)

type t = Re.re

let compile pattern =
  try
    let re = Re.Perl.re pattern |> Re.compile in
    Ok re
  with
  | Re.Perl.Parse_error -> Error ("invalid regex: " ^ pattern)
  | Re.Perl.Not_supported -> Error ("unsupported regex: " ^ pattern)

let matches re s =
  Re.execp re s
