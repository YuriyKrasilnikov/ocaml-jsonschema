(** Format validators: json-pointer, relative-json-pointer (RFC 6901). *)

let check_json_pointer s =
  (* Empty string = valid root pointer *)
  if String.length s = 0 then true
  (* Must start with / *)
  else if s.[0] <> '/' then false
  else
    (* Check each character: ~ must be followed by 0 or 1 *)
    let len = String.length s in
    let ok = ref true in
    let i = ref 0 in
    while !i < len && !ok do
      if s.[!i] = '~' then begin
        if !i + 1 >= len then ok := false
        else if s.[!i + 1] <> '0' && s.[!i + 1] <> '1' then ok := false
        else i := !i + 2
      end else
        i := !i + 1
    done;
    !ok

let check_relative_json_pointer s =
  let len = String.length s in
  if len = 0 then false
  else
    let i = ref 0 in
    while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do incr i done;
    if !i = 0 then false
    else if !i > 1 && s.[0] = '0' then false
    else if !i = len then true
    else if s.[!i] = '#' then !i = len - 1
    else check_json_pointer (String.sub s !i (len - !i))
