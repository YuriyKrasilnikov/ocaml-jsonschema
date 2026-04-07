(** JSON instance data model for JSON Schema. *)

type json = Yojson.Safe.t

let to_float (j : json) =
  match j with
  | `Int n -> Some (Float.of_int n)
  | `Float f -> Some f
  | `Intlit s -> (try Some (Float.of_string s) with _ -> None)
  | _ -> None

let to_int (j : json) =
  match j with
  | `Int n -> Some n
  | `Float f ->
    if Float.is_integer f && Float.is_finite f then
      Some (Float.to_int f)
    else None
  | `Intlit s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let rec json_equal (a : json) (b : json) =
  match a, b with
  | `Null, `Null -> true
  | `Bool a, `Bool b -> a = b
  | `String a, `String b -> String.equal a b
  | `Int a, `Int b -> a = b
  | `Float a, `Float b -> a = b
  | `Int a, `Float b -> Float.equal (Float.of_int a) b
  | `Float a, `Int b -> Float.equal a (Float.of_int b)
  | `Intlit a, `Intlit b -> String.equal a b
  | `Intlit _, (`Int _ | `Float _)
  | (`Int _ | `Float _), `Intlit _ ->
    (match to_float a, to_float b with
     | Some fa, Some fb -> fa = fb
     | _ -> false)
  | `List a, `List b ->
    List.length a = List.length b && List.for_all2 json_equal a b
  | `Assoc a, `Assoc b ->
    List.length a = List.length b &&
    List.for_all (fun (k, v) ->
      match List.assoc_opt k b with
      | Some v' -> json_equal v v'
      | None -> false
    ) a
  | _ -> false

let utf8_length s =
  let len = String.length s in
  let count = ref 0 in
  let i = ref 0 in
  while !i < len do
    let b = Char.code (String.unsafe_get s !i) in
    incr count;
    i := !i +
      (if b land 0x80 = 0 then 1
       else if b land 0xE0 = 0xC0 then 2
       else if b land 0xF0 = 0xE0 then 3
       else 4)
  done;
  !count

let is_type typ (j : json) =
  match typ, j with
  | "null", `Null -> true
  | "boolean", `Bool _ -> true
  | "string", `String _ -> true
  | "array", `List _ -> true
  | "object", `Assoc _ -> true
  | "number", (`Int _ | `Float _ | `Intlit _) -> true
  | "integer", `Int _ -> true
  | "integer", `Intlit _ -> true
  | "integer", `Float f -> Float.is_integer f && Float.is_finite f
  | _ -> false
