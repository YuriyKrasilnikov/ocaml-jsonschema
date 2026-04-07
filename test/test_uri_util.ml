(* Behavioral tests for Uri_util — RFC 3986 URI resolution. *)

open Jsonschema_core

let uri_t = Alcotest.testable Uri.pp Uri.equal

(* ══════════════════════════════════════════════════════════════ *)
(* resolve: RFC 3986 Section 5.4 examples                       *)
(* Base URI: http://a/b/c/d;p?q                                 *)
(* ══════════════════════════════════════════════════════════════ *)

let base = Uri.of_string "http://a/b/c/d;p?q"
let res s = Uri_util.resolve ~base (Uri.of_string s)

(* Normal examples — RFC 3986 Section 5.4.1 *)
let test_resolve_relative_path () =
  Alcotest.(check uri_t) "g:h" (Uri.of_string "g:h") (res "g:h");
  Alcotest.(check uri_t) "g" (Uri.of_string "http://a/b/c/g") (res "g");
  Alcotest.(check uri_t) "./g" (Uri.of_string "http://a/b/c/g") (res "./g");
  Alcotest.(check uri_t) "g/" (Uri.of_string "http://a/b/c/g/") (res "g/");
  Alcotest.(check uri_t) "/g" (Uri.of_string "http://a/g") (res "/g");
  Alcotest.(check uri_t) "?y" (Uri.of_string "http://a/b/c/d;p?y") (res "?y");
  Alcotest.(check uri_t) "g?y" (Uri.of_string "http://a/b/c/g?y") (res "g?y")

let test_resolve_fragment () =
  Alcotest.(check uri_t) "#s" (Uri.of_string "http://a/b/c/d;p?q#s") (res "#s");
  Alcotest.(check uri_t) "g#s" (Uri.of_string "http://a/b/c/g#s") (res "g#s");
  Alcotest.(check uri_t) "g?y#s" (Uri.of_string "http://a/b/c/g?y#s") (res "g?y#s")

let test_resolve_dot_segments () =
  Alcotest.(check uri_t) "../g" (Uri.of_string "http://a/b/g") (res "../g");
  Alcotest.(check uri_t) "../../g" (Uri.of_string "http://a/g") (res "../../g")

let test_resolve_empty () =
  Alcotest.(check uri_t) "empty = base" (Uri.of_string "http://a/b/c/d;p?q") (res "")

(* ══════════════════════════════════════════════════════════════ *)
(* JSON Schema specific: $id resolution                          *)
(* ══════════════════════════════════════════════════════════════ *)

(* From jsonschema-core.xml Section 8.2 examples *)
let test_resolve_id_absolute () =
  let base = Uri.of_string "http://example.com/root.json" in
  let ref_ = Uri.of_string "other.json" in
  Alcotest.(check uri_t) "relative to base"
    (Uri.of_string "http://example.com/other.json")
    (Uri_util.resolve ~base ref_)

let test_resolve_id_nested () =
  let base = Uri.of_string "http://example.com/other.json" in
  let ref_ = Uri.of_string "t/inner.json" in
  Alcotest.(check uri_t) "nested"
    (Uri.of_string "http://example.com/t/inner.json")
    (Uri_util.resolve ~base ref_)

let test_resolve_fragment_only () =
  let base = Uri.of_string "http://example.com/root.json" in
  let ref_ = Uri.of_string "#foo" in
  Alcotest.(check uri_t) "#foo"
    (Uri.of_string "http://example.com/root.json#foo")
    (Uri_util.resolve ~base ref_)

(* ══════════════════════════════════════════════════════════════ *)
(* fragment / without_fragment                                   *)
(* ══════════════════════════════════════════════════════════════ *)

let test_fragment_present () =
  Alcotest.(check (option string)) "has fragment"
    (Some "foo") (Uri_util.fragment (Uri.of_string "http://x.com/a#foo"))

let test_fragment_empty () =
  Alcotest.(check (option string)) "empty fragment"
    (Some "") (Uri_util.fragment (Uri.of_string "http://x.com/a#"))

let test_fragment_absent () =
  Alcotest.(check (option string)) "no fragment"
    None (Uri_util.fragment (Uri.of_string "http://x.com/a"))

let test_without_fragment () =
  Alcotest.(check uri_t) "strip fragment"
    (Uri.of_string "http://x.com/a")
    (Uri_util.without_fragment (Uri.of_string "http://x.com/a#foo"))

(* ══════════════════════════════════════════════════════════════ *)
(* normalize                                                     *)
(* ══════════════════════════════════════════════════════════════ *)

let test_normalize_scheme_case () =
  Alcotest.(check uri_t) "lowercase scheme"
    (Uri.of_string "http://example.com/")
    (Uri_util.normalize (Uri.of_string "HTTP://EXAMPLE.COM/"))

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "uri-util" [
    "resolve", [
      Alcotest.test_case "relative path" `Quick test_resolve_relative_path;
      Alcotest.test_case "fragment" `Quick test_resolve_fragment;
      Alcotest.test_case "dot segments" `Quick test_resolve_dot_segments;
      Alcotest.test_case "empty" `Quick test_resolve_empty;
      Alcotest.test_case "$id absolute" `Quick test_resolve_id_absolute;
      Alcotest.test_case "$id nested" `Quick test_resolve_id_nested;
      Alcotest.test_case "fragment only" `Quick test_resolve_fragment_only;
    ];
    "fragment", [
      Alcotest.test_case "present" `Quick test_fragment_present;
      Alcotest.test_case "empty" `Quick test_fragment_empty;
      Alcotest.test_case "absent" `Quick test_fragment_absent;
      Alcotest.test_case "without_fragment" `Quick test_without_fragment;
    ];
    "normalize", [
      Alcotest.test_case "scheme case" `Quick test_normalize_scheme_case;
    ];
  ]
