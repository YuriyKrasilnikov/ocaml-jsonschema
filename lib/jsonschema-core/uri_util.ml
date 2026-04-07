(** URI utilities for JSON Schema — RFC 3986 resolution. *)

let resolve ~base ref_ =
  Uri.resolve (Uri.scheme base |> Option.value ~default:"") base ref_

let normalize u =
  let scheme = Uri.scheme u |> Option.map String.lowercase_ascii in
  let host = Uri.host u |> Option.map String.lowercase_ascii in
  let u = match scheme with Some s -> Uri.with_scheme u (Some s) | None -> u in
  match host with Some h -> Uri.with_host u (Some h) | None -> u

let fragment u = Uri.fragment u

let without_fragment u = Uri.with_fragment u None
