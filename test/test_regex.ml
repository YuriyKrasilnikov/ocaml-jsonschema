(* Behavioral tests for Regex — JSON Schema regex semantics. *)

open Jsonschema_core

let compile_exn s =
  match Regex.compile s with
  | Ok re -> re
  | Error e -> Alcotest.fail (Printf.sprintf "compile failed: %s" e)

(* ══════════════════════════════════════════════════════════════ *)
(* compile                                                       *)
(* ══════════════════════════════════════════════════════════════ *)

let test_compile_simple () =
  match Regex.compile "^a*$" with
  | Ok _ -> ()
  | Error e -> Alcotest.fail e

let test_compile_invalid () =
  match Regex.compile "[invalid" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for invalid pattern"

(* ══════════════════════════════════════════════════════════════ *)
(* matches: NOT anchored (spec requirement)                      *)
(* ══════════════════════════════════════════════════════════════ *)

(* "the pattern 'es' matches 'expression'" — RFC *)
let test_not_anchored () =
  let re = compile_exn "es" in
  Alcotest.(check bool) "es in expression" true (Regex.matches re "expression")

let test_anchored_explicitly () =
  let re = compile_exn "^abc$" in
  Alcotest.(check bool) "exact" true (Regex.matches re "abc");
  Alcotest.(check bool) "prefix" false (Regex.matches re "abcd");
  Alcotest.(check bool) "suffix" false (Regex.matches re "xabc")

(* Patterns from mandatory test suite *)
let test_pattern_star () =
  let re = compile_exn "^a*$" in
  Alcotest.(check bool) "empty" true (Regex.matches re "");
  Alcotest.(check bool) "aaa" true (Regex.matches re "aaa");
  Alcotest.(check bool) "abc" false (Regex.matches re "abc")

let test_pattern_plus () =
  let re = compile_exn "a+" in
  Alcotest.(check bool) "a" true (Regex.matches re "a");
  Alcotest.(check bool) "bbb" false (Regex.matches re "bbb")

let test_pattern_dot_star () =
  let re = compile_exn "f.*o" in
  Alcotest.(check bool) "foo" true (Regex.matches re "foo");
  Alcotest.(check bool) "fxxxo" true (Regex.matches re "fxxxo");
  Alcotest.(check bool) "bar" false (Regex.matches re "bar")

let test_pattern_char_class () =
  let re = compile_exn "[0-9]{2,}" in
  Alcotest.(check bool) "12" true (Regex.matches re "12");
  Alcotest.(check bool) "1" false (Regex.matches re "1");
  Alcotest.(check bool) "abc" false (Regex.matches re "abc")

let test_pattern_bar_anchor () =
  let re = compile_exn "^.*bar$" in
  Alcotest.(check bool) "bar" true (Regex.matches re "bar");
  Alcotest.(check bool) "foobar" true (Regex.matches re "foobar");
  Alcotest.(check bool) "barbaz" false (Regex.matches re "barbaz")

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "regex" [
    "compile", [
      Alcotest.test_case "simple" `Quick test_compile_simple;
      Alcotest.test_case "invalid" `Quick test_compile_invalid;
    ];
    "matches", [
      Alcotest.test_case "not anchored" `Quick test_not_anchored;
      Alcotest.test_case "anchored explicitly" `Quick test_anchored_explicitly;
      Alcotest.test_case "star" `Quick test_pattern_star;
      Alcotest.test_case "plus" `Quick test_pattern_plus;
      Alcotest.test_case "dot star" `Quick test_pattern_dot_star;
      Alcotest.test_case "char class" `Quick test_pattern_char_class;
      Alcotest.test_case "bar anchor" `Quick test_pattern_bar_anchor;
    ];
  ]
