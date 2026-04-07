(** Format validators: uri, uri-reference, iri, iri-reference, uri-template. *)

let has_scheme s =
  (* scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) ":" *)
  let len = String.length s in
  if len = 0 then false
  else
    let c = s.[0] in
    if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then false
    else
      try
        let colon = String.index s ':' in
        let ok = ref true in
        for i = 1 to colon - 1 do
          let c = s.[i] in
          if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
                  || (c >= '0' && c <= '9') || c = '+' || c = '-' || c = '.') then
            ok := false
        done;
        !ok && colon > 0
      with Not_found -> false

let is_hex c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let check_percent_encoding s =
  let len = String.length s in
  let ok = ref true in
  let i = ref 0 in
  while !i < len do
    if s.[!i] = '%' then begin
      if !i + 2 >= len then ok := false
      else if not (is_hex s.[!i+1] && is_hex s.[!i+2]) then ok := false
      else i := !i + 2
    end;
    incr i
  done;
  !ok

let no_invalid_chars s =
  let ok = ref true in
  String.iter (fun c ->
    if c = ' ' || c = '\t' || c = '\n' || c = '\r' then ok := false
    else if c = '{' || c = '}' || c = '|' || c = '\\' || c = '^'
            || c = '`' then ok := false
    else if c = '<' || c = '>' || c = '"' then ok := false
    else if c = '[' && not (String.contains s ']') then ok := false
  ) s;
  !ok

let is_ascii_only s =
  let ok = ref true in
  String.iter (fun c -> if Char.code c > 127 then ok := false) s;
  !ok

let check_port s =
  (* Find authority after scheme "://" *)
  try
    let dslash = Str.search_forward (Str.regexp "://") s 0 in
    let auth_start = dslash + 3 in
    let rest = String.sub s auth_start (String.length s - auth_start) in
    let auth_end = try String.index rest '/' with Not_found ->
                   try String.index rest '?' with Not_found ->
                   try String.index rest '#' with Not_found -> String.length rest in
    let authority = String.sub rest 0 auth_end in
    (* Skip userinfo *)
    let host_start = try String.index authority '@' + 1 with Not_found -> 0 in
    let host_part = String.sub authority host_start (String.length authority - host_start) in
    (* Skip IPv6 brackets *)
    if String.length host_part > 0 && host_part.[0] = '[' then true
    else
      match String.rindex_opt host_part ':' with
      | None -> true
      | Some colon_pos ->
        let port_str = String.sub host_part (colon_pos + 1) (String.length host_part - colon_pos - 1) in
        if String.length port_str = 0 then true
        else
          String.to_seq port_str |> Seq.for_all (fun c -> c >= '0' && c <= '9')
  with Not_found -> true

let check_uri s =
  String.length s > 0
  && has_scheme s
  && no_invalid_chars s
  && is_ascii_only s
  && check_percent_encoding s
  && check_port s

let check_uri_reference s =
  String.length s >= 0
  && no_invalid_chars s
  && is_ascii_only s
  && check_percent_encoding s

let no_invalid_iri_chars s =
  let ok = ref true in
  String.iter (fun c ->
    if c = '\\' then ok := false
    else if c = ' ' || c = '\t' || c = '\n' || c = '\r' then ok := false
  ) s;
  !ok

let check_ipv6_brackets s =
  (* If authority has multiple colons, IPv6 must be in brackets *)
  try
    let dslash = Str.search_forward (Str.regexp "://") s 0 in
    let auth_start = dslash + 3 in
    let rest = String.sub s auth_start (String.length s - auth_start) in
    let auth_end = try String.index rest '/' with Not_found ->
                   try String.index rest '?' with Not_found ->
                   try String.index rest '#' with Not_found -> String.length rest in
    let authority = String.sub rest 0 auth_end in
    let host_start = try String.index authority '@' + 1 with Not_found -> 0 in
    let host_part = String.sub authority host_start (String.length authority - host_start) in
    (* Count colons — more than 1 without brackets = invalid IPv6 *)
    let colon_count = String.fold_left (fun acc c -> if c = ':' then acc + 1 else acc) 0 host_part in
    if colon_count > 1 && not (String.contains host_part '[') then false
    else true
  with Not_found -> true

let check_iri s =
  String.length s > 0 && has_scheme s && no_invalid_iri_chars s
  && check_percent_encoding s && check_ipv6_brackets s

let check_iri_reference s =
  String.length s > 0 && no_invalid_iri_chars s
  && check_percent_encoding s

let check_uri_template s =
  (* RFC 6570: basic structural check *)
  let len = String.length s in
  if len = 0 then true
  else
    (* Check balanced braces *)
    let depth = ref 0 in
    let ok = ref true in
    String.iter (fun c ->
      if c = '{' then incr depth
      else if c = '}' then begin
        decr depth;
        if !depth < 0 then ok := false
      end
    ) s;
    !ok && !depth = 0
