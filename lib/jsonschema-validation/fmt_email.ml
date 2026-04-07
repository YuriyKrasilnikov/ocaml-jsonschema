(** Format validators: email, idn-email (RFC 5322 §3.4.1). *)

let check_email s =
  (* Must have exactly one @ *)
  match String.split_on_char '@' s with
  | [local; domain] ->
    let llen = String.length local in
    let dlen = String.length domain in
    if llen = 0 || llen > 64 || dlen = 0 || dlen > 253 then false
    (* No leading/trailing dots in local part *)
    else if local.[0] = '.' || local.[llen - 1] = '.' then false
    (* No consecutive dots in local part *)
    else if (try ignore (Str.search_forward (Str.regexp_string "..") local 0); true
             with Not_found -> false) then false
    (* Local part: only printable ASCII, no spaces or special chars unquoted *)
    else if String.contains local ' ' then false
    (* Must not look like a full header "Name <addr>" *)
    else if String.contains s '<' || String.contains s '>' then false
    (* Domain must have at least one dot *)
    else if not (String.contains domain '.') then false
    (* Domain labels basic check *)
    else
      let labels = String.split_on_char '.' domain in
      List.for_all (fun l -> String.length l > 0) labels
  | _ -> false

let check_idn_email s =
  check_email s
