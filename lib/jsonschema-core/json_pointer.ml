(** JSON Pointer (RFC 6901). *)

type t = string list

let empty = []

let unescape token =
  let buf = Buffer.create (String.length token) in
  let len = String.length token in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && token.[!i] = '~' then begin
      (match token.[!i + 1] with
       | '0' -> Buffer.add_char buf '~'
       | '1' -> Buffer.add_char buf '/'
       | c -> Buffer.add_char buf '~'; Buffer.add_char buf c);
      i := !i + 2
    end else begin
      Buffer.add_char buf token.[!i];
      i := !i + 1
    end
  done;
  Buffer.contents buf

let escape token =
  let buf = Buffer.create (String.length token) in
  String.iter (fun c ->
    match c with
    | '~' -> Buffer.add_string buf "~0"
    | '/' -> Buffer.add_string buf "~1"
    | c -> Buffer.add_char buf c
  ) token;
  Buffer.contents buf

let of_string s =
  if String.length s = 0 then Ok []
  else if s.[0] <> '/' then Error ("JSON Pointer must start with /: " ^ s)
  else
    let rest = String.sub s 1 (String.length s - 1) in
    let tokens = String.split_on_char '/' rest in
    Ok (List.map unescape tokens)

let to_string t =
  match t with
  | [] -> ""
  | _ -> "/" ^ String.concat "/" (List.map escape t)

let resolve t json =
  let step json token =
    match json with
    | `Assoc pairs ->
      (match List.assoc_opt token pairs with
       | Some v -> Some v
       | None -> None)
    | `List items ->
      (match int_of_string_opt token with
       | Some i when i >= 0 && i < List.length items ->
         Some (List.nth items i)
       | _ -> None)
    | _ -> None
  in
  List.fold_left (fun acc token ->
    match acc with
    | None -> None
    | Some json -> step json token
  ) (Some json) t

let append t token = t @ [token]
