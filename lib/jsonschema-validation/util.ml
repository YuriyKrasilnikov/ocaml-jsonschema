(** Shared utilities for keyword compilers. *)

(** Extract a non-negative integer from a schema value.
    Handles both Int and Float with zero fractional part
    (spec allows 2.0 as a valid integer keyword value). *)
let to_nonneg_int (j : Yojson.Safe.t) =
  match j with
  | `Int n when n >= 0 -> Some n
  | `Float f when f >= 0.0 && Float.is_integer f -> Some (Float.to_int f)
  | _ -> None
