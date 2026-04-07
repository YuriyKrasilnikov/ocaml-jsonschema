(** Keywords: contentEncoding, contentMediaType (spec §8). *)

open Jsonschema_core

let is_valid_base64 s =
  let len = String.length s in
  if len = 0 then true
  else
    let ok = ref true in
    let pad_start = ref len in
    (* Find padding start *)
    let i = ref (len - 1) in
    while !i >= 0 && s.[!i] = '=' do pad_start := !i; decr i done;
    (* Check base64 characters *)
    for j = 0 to !pad_start - 1 do
      let c = s.[j] in
      if not ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
              || (c >= '0' && c <= '9') || c = '+' || c = '/') then
        ok := false
    done;
    (* Check padding is valid (0-2 '=' chars at end) *)
    let pad_len = len - !pad_start in
    if pad_len > 2 then ok := false;
    (* Length must be multiple of 4 *)
    if len mod 4 <> 0 then ok := false;
    !ok

let decode_base64 s =
  let tbl c =
    if c >= 'A' && c <= 'Z' then Char.code c - 65
    else if c >= 'a' && c <= 'z' then Char.code c - 97 + 26
    else if c >= '0' && c <= '9' then Char.code c - 48 + 52
    else if c = '+' then 62
    else if c = '/' then 63
    else 0
  in
  let len = String.length s in
  let buf = Buffer.create (len * 3 / 4) in
  let i = ref 0 in
  while !i + 3 < len do
    let a = tbl s.[!i] and b = tbl s.[!i+1] and c = tbl s.[!i+2] and d = tbl s.[!i+3] in
    Buffer.add_char buf (Char.chr ((a lsl 2) lor (b lsr 4)));
    if s.[!i+2] <> '=' then
      Buffer.add_char buf (Char.chr (((b land 0xF) lsl 4) lor (c lsr 2)));
    if s.[!i+3] <> '=' then
      Buffer.add_char buf (Char.chr (((c land 0x3) lsl 6) lor d));
    i := !i + 4
  done;
  Buffer.contents buf

let is_valid_json s =
  try
    let _ = Yojson.Basic.from_string s in true
  with _ -> false

let compile_content_encoding : Compiler.keyword_compiler = {
  keyword = "contentEncoding";
  compile = fun ctx value ->
    match value with
    | `String "base64" ->
      Some (fun instance path ->
        match instance with
        | `String s ->
          if is_valid_base64 s then []
          else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                  ~keyword:"contentEncoding" "invalid base64"]
        | _ -> [])
    | _ -> None;
}

let compile_content_media_type : Compiler.keyword_compiler = {
  keyword = "contentMediaType";
  compile = fun ctx value ->
    match value with
    | `String "application/json" ->
      Some (fun instance path ->
        match instance with
        | `String s ->
          (* Check if schema also has contentEncoding *)
          let has_encoding = match ctx.schema_obj with
            | `Assoc pairs -> List.mem_assoc "contentEncoding" pairs
            | _ -> false
          in
          let to_validate = if has_encoding then
            (* Decode first, then validate JSON *)
            let encoding = match ctx.schema_obj with
              | `Assoc pairs -> (match List.assoc_opt "contentEncoding" pairs with
                | Some (`String e) -> e | _ -> "")
              | _ -> "" in
            if encoding = "base64" then
              if is_valid_base64 s then Some (decode_base64 s) else None
            else Some s
          else Some s in
          (match to_validate with
           | None -> []  (* encoding invalid, handled by contentEncoding *)
           | Some decoded ->
             if is_valid_json decoded then []
             else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                     ~keyword:"contentMediaType" "invalid JSON content"])
        | _ -> [])
    | _ -> None;
}

let compilers = [compile_content_encoding; compile_content_media_type]
