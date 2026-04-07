(* Behavioral tests for Json_pointer (RFC 6901). *)

open Jsonschema_core

(* RFC 6901 Section 5 example document *)
let rfc_doc =
  Yojson.Safe.from_string {|
  {
    "foo": ["bar", "baz"],
    "": 0,
    "a/b": 1,
    "c%d": 2,
    "e^f": 3,
    "g|h": 4,
    "i\\j": 5,
    "k\"l": 6,
    " ": 7,
    "m~n": 8
  }|}

(* ══════════════════════════════════════════════════════════════ *)
(* of_string: parsing                                            *)
(* ══════════════════════════════════════════════════════════════ *)

(* Happy path: RFC 6901 Section 5 examples *)
let test_parse_empty () =
  Alcotest.(check (result (list string) string)) "root"
    (Ok []) (Json_pointer.of_string "")

let test_parse_foo () =
  Alcotest.(check (result (list string) string)) "/foo"
    (Ok ["foo"]) (Json_pointer.of_string "/foo")

let test_parse_foo_0 () =
  Alcotest.(check (result (list string) string)) "/foo/0"
    (Ok ["foo"; "0"]) (Json_pointer.of_string "/foo/0")

let test_parse_empty_key () =
  Alcotest.(check (result (list string) string)) "/"
    (Ok [""]) (Json_pointer.of_string "/")

let test_parse_slash_in_key () =
  Alcotest.(check (result (list string) string)) "/a~1b"
    (Ok ["a/b"]) (Json_pointer.of_string "/a~1b")

let test_parse_tilde_in_key () =
  Alcotest.(check (result (list string) string)) "/m~0n"
    (Ok ["m~n"]) (Json_pointer.of_string "/m~0n")

let test_parse_multiple_segments () =
  Alcotest.(check (result (list string) string)) "/a/b/c"
    (Ok ["a"; "b"; "c"]) (Json_pointer.of_string "/a/b/c")

(* Boundary: escape order matters — ~01 = "~1" not "/" *)
let test_parse_tilde_then_one () =
  Alcotest.(check (result (list string) string)) "~01 = literal ~1"
    (Ok ["~1"]) (Json_pointer.of_string "/~01")

(* Negative: must start with / *)
let test_parse_no_leading_slash () =
  match Json_pointer.of_string "foo" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for missing leading /"

(* ══════════════════════════════════════════════════════════════ *)
(* to_string: serialization                                      *)
(* ══════════════════════════════════════════════════════════════ *)

(* Roundtrip: parse then serialize = original *)
let test_to_string_empty () =
  Alcotest.(check string) "root" "" (Json_pointer.to_string [])

let test_to_string_foo () =
  Alcotest.(check string) "/foo" "/foo" (Json_pointer.to_string ["foo"])

let test_to_string_foo_0 () =
  Alcotest.(check string) "/foo/0" "/foo/0" (Json_pointer.to_string ["foo"; "0"])

let test_to_string_escapes_slash () =
  Alcotest.(check string) "a/b" "/a~1b" (Json_pointer.to_string ["a/b"])

let test_to_string_escapes_tilde () =
  Alcotest.(check string) "m~n" "/m~0n" (Json_pointer.to_string ["m~n"])

(* Roundtrip: of_string → to_string = identity *)
let test_roundtrip () =
  let inputs = [""; "/foo"; "/foo/0"; "/"; "/a~1b"; "/c%d"; "/e^f";
                 "/g|h"; "/i\\j"; "/k\"l"; "/ "; "/m~0n"] in
  List.iter (fun s ->
    match Json_pointer.of_string s with
    | Ok t ->
      Alcotest.(check string) (Printf.sprintf "roundtrip %s" s)
        s (Json_pointer.to_string t)
    | Error e -> Alcotest.fail (Printf.sprintf "parse failed for %s: %s" s e)
  ) inputs

(* ══════════════════════════════════════════════════════════════ *)
(* resolve: RFC 6901 Section 5 full example                      *)
(* ══════════════════════════════════════════════════════════════ *)

let json_t = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal
let opt_json = Alcotest.option json_t

(* Helper: parse and resolve *)
let resolve_str s doc =
  match Json_pointer.of_string s with
  | Ok t -> Json_pointer.resolve t doc
  | Error e -> Alcotest.fail (Printf.sprintf "parse: %s" e)

(* RFC 6901 Section 5: all examples *)
let test_resolve_root () =
  Alcotest.(check opt_json) "whole doc"
    (Some rfc_doc) (resolve_str "" rfc_doc)

let test_resolve_foo () =
  Alcotest.(check opt_json) "/foo"
    (Some (`List [`String "bar"; `String "baz"]))
    (resolve_str "/foo" rfc_doc)

let test_resolve_foo_0 () =
  Alcotest.(check opt_json) "/foo/0"
    (Some (`String "bar"))
    (resolve_str "/foo/0" rfc_doc)

let test_resolve_empty_key () =
  Alcotest.(check opt_json) "/"
    (Some (`Int 0)) (resolve_str "/" rfc_doc)

let test_resolve_slash_in_key () =
  Alcotest.(check opt_json) "/a~1b"
    (Some (`Int 1)) (resolve_str "/a~1b" rfc_doc)

let test_resolve_percent () =
  Alcotest.(check opt_json) "/c%d"
    (Some (`Int 2)) (resolve_str "/c%d" rfc_doc)

let test_resolve_caret () =
  Alcotest.(check opt_json) "/e^f"
    (Some (`Int 3)) (resolve_str "/e^f" rfc_doc)

let test_resolve_pipe () =
  Alcotest.(check opt_json) "/g|h"
    (Some (`Int 4)) (resolve_str "/g|h" rfc_doc)

let test_resolve_backslash () =
  Alcotest.(check opt_json) "/i\\j"
    (Some (`Int 5)) (resolve_str "/i\\j" rfc_doc)

let test_resolve_quote () =
  Alcotest.(check opt_json) "/k\"l"
    (Some (`Int 6)) (resolve_str "/k\"l" rfc_doc)

let test_resolve_space () =
  Alcotest.(check opt_json) "/ "
    (Some (`Int 7)) (resolve_str "/ " rfc_doc)

let test_resolve_tilde () =
  Alcotest.(check opt_json) "/m~0n"
    (Some (`Int 8)) (resolve_str "/m~0n" rfc_doc)

(* Boundary: array index *)
let test_resolve_array_index () =
  let doc = Yojson.Safe.from_string {|{"a": [10, 20, 30]}|} in
  Alcotest.(check opt_json) "/a/2"
    (Some (`Int 30)) (resolve_str "/a/2" doc)

(* Boundary: nested objects *)
let test_resolve_nested () =
  let doc = Yojson.Safe.from_string {|{"a": {"b": {"c": true}}}|} in
  Alcotest.(check opt_json) "/a/b/c"
    (Some (`Bool true)) (resolve_str "/a/b/c" doc)

(* Negative: nonexistent key → None *)
let test_resolve_missing_key () =
  Alcotest.(check opt_json) "missing"
    None (resolve_str "/nonexistent" rfc_doc)

(* Negative: array index out of bounds → None *)
let test_resolve_array_oob () =
  Alcotest.(check opt_json) "oob"
    None (resolve_str "/foo/99" rfc_doc)

(* Negative: non-numeric index on array → None *)
let test_resolve_non_numeric_index () =
  Alcotest.(check opt_json) "non-numeric"
    None (resolve_str "/foo/bar" rfc_doc)

(* Negative: descend into scalar → None *)
let test_resolve_into_scalar () =
  let doc = Yojson.Safe.from_string {|{"a": 42}|} in
  Alcotest.(check opt_json) "into scalar"
    None (resolve_str "/a/b" doc)

(* ══════════════════════════════════════════════════════════════ *)
(* append                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let test_append_to_empty () =
  Alcotest.(check (list string)) "append to root"
    ["foo"] (Json_pointer.append Json_pointer.empty "foo")

let test_append_to_existing () =
  Alcotest.(check (list string)) "append"
    ["foo"; "bar"] (Json_pointer.append ["foo"] "bar")

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "json-pointer" [
    "of_string", [
      Alcotest.test_case "empty" `Quick test_parse_empty;
      Alcotest.test_case "/foo" `Quick test_parse_foo;
      Alcotest.test_case "/foo/0" `Quick test_parse_foo_0;
      Alcotest.test_case "/ (empty key)" `Quick test_parse_empty_key;
      Alcotest.test_case "/a~1b (slash)" `Quick test_parse_slash_in_key;
      Alcotest.test_case "/m~0n (tilde)" `Quick test_parse_tilde_in_key;
      Alcotest.test_case "multiple segments" `Quick test_parse_multiple_segments;
      Alcotest.test_case "~01 escape order" `Quick test_parse_tilde_then_one;
      Alcotest.test_case "no leading slash" `Quick test_parse_no_leading_slash;
    ];
    "to_string", [
      Alcotest.test_case "empty" `Quick test_to_string_empty;
      Alcotest.test_case "/foo" `Quick test_to_string_foo;
      Alcotest.test_case "/foo/0" `Quick test_to_string_foo_0;
      Alcotest.test_case "escapes slash" `Quick test_to_string_escapes_slash;
      Alcotest.test_case "escapes tilde" `Quick test_to_string_escapes_tilde;
      Alcotest.test_case "roundtrip" `Quick test_roundtrip;
    ];
    "resolve", [
      Alcotest.test_case "root" `Quick test_resolve_root;
      Alcotest.test_case "/foo" `Quick test_resolve_foo;
      Alcotest.test_case "/foo/0" `Quick test_resolve_foo_0;
      Alcotest.test_case "/ (empty key)" `Quick test_resolve_empty_key;
      Alcotest.test_case "/a~1b" `Quick test_resolve_slash_in_key;
      Alcotest.test_case "/c%d" `Quick test_resolve_percent;
      Alcotest.test_case "/e^f" `Quick test_resolve_caret;
      Alcotest.test_case "/g|h" `Quick test_resolve_pipe;
      Alcotest.test_case "/i\\j" `Quick test_resolve_backslash;
      Alcotest.test_case "/k\"l" `Quick test_resolve_quote;
      Alcotest.test_case "/ (space)" `Quick test_resolve_space;
      Alcotest.test_case "/m~0n" `Quick test_resolve_tilde;
      Alcotest.test_case "array index" `Quick test_resolve_array_index;
      Alcotest.test_case "nested objects" `Quick test_resolve_nested;
      Alcotest.test_case "missing key" `Quick test_resolve_missing_key;
      Alcotest.test_case "array oob" `Quick test_resolve_array_oob;
      Alcotest.test_case "non-numeric index" `Quick test_resolve_non_numeric_index;
      Alcotest.test_case "into scalar" `Quick test_resolve_into_scalar;
    ];
    "append", [
      Alcotest.test_case "to empty" `Quick test_append_to_empty;
      Alcotest.test_case "to existing" `Quick test_append_to_existing;
    ];
  ]
