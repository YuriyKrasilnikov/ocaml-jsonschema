(** Keyword: format (spec §7).

    Dispatches to format-specific validators.
    Only validates when format_assertion=true in config. *)

open Jsonschema_core

let compile_format : Compiler.keyword_compiler = {
  keyword = "format";
  compile = fun ctx value ->
    if not ctx.config.format_assertion then None
    else match value with
    | `String format_name ->
      let check = match format_name with
        | "date-time" -> Some Fmt_datetime.check_datetime
        | "date" -> Some Fmt_datetime.check_date
        | "time" -> Some Fmt_datetime.check_time
        | "email" -> Some Fmt_email.check_email
        | "idn-email" -> Some Fmt_email.check_idn_email
        | "hostname" -> Some Fmt_hostname.check_hostname
        | "idn-hostname" -> Some Fmt_hostname.check_idn_hostname
        | "ipv4" -> Some Fmt_ip.check_ipv4
        | "ipv6" -> Some Fmt_ip.check_ipv6
        | "uri" -> Some Fmt_uri.check_uri
        | "uri-reference" -> Some Fmt_uri.check_uri_reference
        | "iri" -> Some Fmt_uri.check_iri
        | "iri-reference" -> Some Fmt_uri.check_iri_reference
        | "uri-template" -> Some Fmt_uri.check_uri_template
        | "json-pointer" -> Some Fmt_pointer.check_json_pointer
        | "relative-json-pointer" -> Some Fmt_pointer.check_relative_json_pointer
        | "regex" -> Some Fmt_regex.check_regex
        | _ -> None  (* unknown format — pass *)
      in
      (match check with
       | Some validator_fn ->
         Some (fun instance path ->
           match instance with
           | `String s ->
             if validator_fn s then []
             else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                     ~keyword:"format"
                     (Printf.sprintf "invalid %s: %s" format_name s)]
           | _ -> [])  (* format only applies to strings *)
       | None -> None)
    | _ -> None;
}

let compilers = [compile_format]
