(** JSON Schema representation.

    A schema MUST be an object or a boolean. *)

type t =
  | Bool of bool
  | Obj of Yojson.Safe.t

let of_json (j : Yojson.Safe.t) =
  match j with
  | `Bool b -> Ok (Bool b)
  | `Assoc _ -> Ok (Obj j)
  | _ -> Error "schema must be an object or a boolean"

let keyword key = function
  | Bool _ -> None
  | Obj (`Assoc pairs) -> List.assoc_opt key pairs
  | Obj _ -> None

let keywords = function
  | Bool _ -> []
  | Obj (`Assoc pairs) -> List.map fst pairs
  | Obj _ -> []
