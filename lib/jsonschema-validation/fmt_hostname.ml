(** Format validators: hostname, idn-hostname.
    Uses ocaml-idna for IDNA2008 validation. *)

let check_hostname s =
  let len = String.length s in
  if len = 0 || len > 253 then false
  else if s.[len - 1] = '.' then false
  else if s.[0] = '.' then false
  (* Must be pure ASCII for hostname format *)
  else if String.length s <> Jsonschema_core.Instance.utf8_length s then false
  else Idna.is_valid_hostname s

(** Replace Unicode dot separators with ASCII dot.
    U+3002 IDEOGRAPHIC FULL STOP, U+FF0E FULLWIDTH FULL STOP,
    U+FF61 HALFWIDTH IDEOGRAPHIC FULL STOP. *)
let normalize_dots s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    let b = Char.code s.[!i] in
    if b = 0xE3 && !i + 2 < len
       && Char.code s.[!i+1] = 0x80 && Char.code s.[!i+2] = 0x82 then begin
      (* U+3002 = E3 80 82 *)
      Buffer.add_char buf '.'; i := !i + 3
    end else if b = 0xEF && !i + 2 < len then begin
      let b1 = Char.code s.[!i+1] in
      let b2 = Char.code s.[!i+2] in
      if b1 = 0xBC && b2 = 0x8E then begin
        (* U+FF0E = EF BC 8E *)
        Buffer.add_char buf '.'; i := !i + 3
      end else if b1 = 0xBD && b2 = 0xA1 then begin
        (* U+FF61 = EF BD A1 *)
        Buffer.add_char buf '.'; i := !i + 3
      end else begin
        Buffer.add_char buf s.[!i]; incr i
      end
    end else begin
      Buffer.add_char buf s.[!i]; incr i
    end
  done;
  Buffer.contents buf

let check_idn_hostname s =
  let s = normalize_dots s in
  let len = String.length s in
  if len = 0 || len > 253 then false
  else if s.[len - 1] = '.' then false
  else if s.[0] = '.' then false
  else Idna.is_valid_hostname s
