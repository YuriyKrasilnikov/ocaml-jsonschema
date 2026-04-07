(** Format validators: ipv4, ipv6. Uses ipaddr library. *)

let check_ipv4 s =
  (* Strict: no leading zeros, exactly 4 octets 0-255 *)
  match Ipaddr.V4.of_string s with
  | Ok ip ->
    (* ipaddr is lenient — re-serialize and compare to reject leading zeros *)
    String.equal (Ipaddr.V4.to_string ip) s
  | Error _ -> false

let check_ipv6 s =
  match Ipaddr.V6.of_string s with
  | Ok _ -> true
  | Error _ -> false
