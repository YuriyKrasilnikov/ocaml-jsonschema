(* Behavioral tests for Instance — JSON Schema data model. *)

open Jsonschema_core

(* ══════════════════════════════════════════════════════════════ *)
(* json_equal                                                    *)
(* ══════════════════════════════════════════════════════════════ *)

let eq a b = Instance.json_equal a b
let j s = Yojson.Safe.from_string s

(* Happy path: same types *)
let test_equal_null () =
  Alcotest.(check bool) "null=null" true (eq `Null `Null)

let test_equal_bool () =
  Alcotest.(check bool) "true=true" true (eq (`Bool true) (`Bool true));
  Alcotest.(check bool) "true<>false" false (eq (`Bool true) (`Bool false))

let test_equal_string () =
  Alcotest.(check bool) "same" true (eq (`String "abc") (`String "abc"));
  Alcotest.(check bool) "diff" false (eq (`String "abc") (`String "xyz"))

let test_equal_int () =
  Alcotest.(check bool) "42=42" true (eq (`Int 42) (`Int 42));
  Alcotest.(check bool) "1<>2" false (eq (`Int 1) (`Int 2))

let test_equal_float () =
  Alcotest.(check bool) "1.5=1.5" true (eq (`Float 1.5) (`Float 1.5));
  Alcotest.(check bool) "1.0<>1.1" false (eq (`Float 1.0) (`Float 1.1))

(* Key: Int and Float with same mathematical value are EQUAL *)
let test_equal_int_float () =
  Alcotest.(check bool) "Int 1 = Float 1.0" true (eq (`Int 1) (`Float 1.0));
  Alcotest.(check bool) "Float 1.0 = Int 1" true (eq (`Float 1.0) (`Int 1));
  Alcotest.(check bool) "Int 0 = Float 0.0" true (eq (`Int 0) (`Float 0.0));
  Alcotest.(check bool) "Int(-2) = Float(-2.0)" true (eq (`Int (-2)) (`Float (-2.0)));
  Alcotest.(check bool) "Int 1 <> Float 1.5" false (eq (`Int 1) (`Float 1.5))

(* Key: Bool is NEVER equal to number *)
let test_equal_bool_vs_number () =
  Alcotest.(check bool) "true<>1" false (eq (`Bool true) (`Int 1));
  Alcotest.(check bool) "false<>0" false (eq (`Bool false) (`Int 0));
  Alcotest.(check bool) "true<>1.0" false (eq (`Bool true) (`Float 1.0));
  Alcotest.(check bool) "false<>0.0" false (eq (`Bool false) (`Float 0.0))

(* Different types *)
let test_equal_diff_types () =
  Alcotest.(check bool) "null<>0" false (eq `Null (`Int 0));
  Alcotest.(check bool) "str<>int" false (eq (`String "1") (`Int 1));
  Alcotest.(check bool) "null<>false" false (eq `Null (`Bool false))

(* Arrays *)
let test_equal_arrays () =
  Alcotest.(check bool) "same" true (eq (j "[1,2,3]") (j "[1,2,3]"));
  Alcotest.(check bool) "int/float" true (eq (j "[1, 2]") (j "[1.0, 2.0]"));
  Alcotest.(check bool) "diff order" false (eq (j "[1,2]") (j "[2,1]"));
  Alcotest.(check bool) "diff len" false (eq (j "[1]") (j "[1,2]"))

(* Objects: unordered *)
let test_equal_objects () =
  Alcotest.(check bool) "same" true
    (eq (j {|{"a":1,"b":2}|}) (j {|{"b":2,"a":1}|}));
  Alcotest.(check bool) "int/float" true
    (eq (j {|{"x":1}|}) (j {|{"x":1.0}|}));
  Alcotest.(check bool) "diff val" false
    (eq (j {|{"a":1}|}) (j {|{"a":2}|}));
  Alcotest.(check bool) "diff key" false
    (eq (j {|{"a":1}|}) (j {|{"b":1}|}));
  Alcotest.(check bool) "extra key" false
    (eq (j {|{"a":1}|}) (j {|{"a":1,"b":2}|}))

(* Deep equality: bool vs number inside containers *)
let test_equal_deep_bool_number () =
  Alcotest.(check bool) "[false]<>[0]" false (eq (j "[false]") (j "[0]"));
  Alcotest.(check bool) "[true]<>[1]" false (eq (j "[true]") (j "[1]"));
  Alcotest.(check bool) "{a:false}<>{a:0}" false
    (eq (j {|{"a":false}|}) (j {|{"a":0}|}))

(* Unicode string equality: codepoint-exact, no normalization *)
let test_equal_unicode_no_normalization () =
  (* U+03BC (mu) vs U+00B5 (micro sign) — visually same, different codepoints *)
  Alcotest.(check bool) "mu<>micro"
    false (eq (`String "\xCE\xBC") (`String "\xC2\xB5"))

(* ══════════════════════════════════════════════════════════════ *)
(* utf8_length                                                   *)
(* ══════════════════════════════════════════════════════════════ *)

let test_utf8_ascii () =
  Alcotest.(check int) "ascii" 3 (Instance.utf8_length "foo")

let test_utf8_empty () =
  Alcotest.(check int) "empty" 0 (Instance.utf8_length "")

let test_utf8_2byte () =
  (* "ü" = U+00FC = 2 bytes UTF-8 *)
  Alcotest.(check int) "umlaut" 1 (Instance.utf8_length "\xC3\xBC")

let test_utf8_3byte () =
  (* "€" = U+20AC = 3 bytes UTF-8 *)
  Alcotest.(check int) "euro" 1 (Instance.utf8_length "\xE2\x82\xAC")

let test_utf8_4byte () =
  (* Poop emoji U+1F4A9 = 4 bytes UTF-8 *)
  Alcotest.(check int) "poop" 1 (Instance.utf8_length "\xF0\x9F\x92\xA9")

let test_utf8_mixed () =
  (* "aüb" = 3 codepoints, 4 bytes *)
  Alcotest.(check int) "mixed" 3 (Instance.utf8_length "a\xC3\xBCb")

let test_utf8_two_emoji () =
  (* Two poop emojis = 2 codepoints, 8 bytes *)
  Alcotest.(check int) "two emoji" 2
    (Instance.utf8_length "\xF0\x9F\x92\xA9\xF0\x9F\x92\xA9")

(* ══════════════════════════════════════════════════════════════ *)
(* to_float                                                      *)
(* ══════════════════════════════════════════════════════════════ *)

let opt_float = Alcotest.(option (float 0.0))

let test_to_float_int () =
  Alcotest.(check opt_float) "int" (Some 42.0) (Instance.to_float (`Int 42))

let test_to_float_float () =
  Alcotest.(check opt_float) "float" (Some 1.5) (Instance.to_float (`Float 1.5))

let test_to_float_intlit () =
  Alcotest.(check opt_float) "intlit"
    (Some 123.0) (Instance.to_float (`Intlit "123"))

let test_to_float_non_number () =
  Alcotest.(check opt_float) "string" None (Instance.to_float (`String "42"));
  Alcotest.(check opt_float) "null" None (Instance.to_float `Null);
  Alcotest.(check opt_float) "bool" None (Instance.to_float (`Bool true))

(* ══════════════════════════════════════════════════════════════ *)
(* to_int                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let opt_int = Alcotest.(option int)

let test_to_int_int () =
  Alcotest.(check opt_int) "int" (Some 42) (Instance.to_int (`Int 42))

let test_to_int_float_integer () =
  Alcotest.(check opt_int) "1.0" (Some 1) (Instance.to_int (`Float 1.0))

let test_to_int_float_non_integer () =
  Alcotest.(check opt_int) "1.5" None (Instance.to_int (`Float 1.5))

let test_to_int_non_number () =
  Alcotest.(check opt_int) "string" None (Instance.to_int (`String "1"))

(* ══════════════════════════════════════════════════════════════ *)
(* is_type                                                       *)
(* ══════════════════════════════════════════════════════════════ *)

let test_type_null () =
  Alcotest.(check bool) "null" true (Instance.is_type "null" `Null);
  Alcotest.(check bool) "not null" false (Instance.is_type "null" (`Int 0))

let test_type_boolean () =
  Alcotest.(check bool) "bool" true (Instance.is_type "boolean" (`Bool true));
  Alcotest.(check bool) "not bool" false (Instance.is_type "boolean" (`Int 1))

let test_type_string () =
  Alcotest.(check bool) "string" true (Instance.is_type "string" (`String "x"));
  Alcotest.(check bool) "not string" false (Instance.is_type "string" (`Int 1))

let test_type_array () =
  Alcotest.(check bool) "array" true (Instance.is_type "array" (`List []));
  Alcotest.(check bool) "not array" false (Instance.is_type "array" (`Int 1))

let test_type_object () =
  Alcotest.(check bool) "object" true (Instance.is_type "object" (`Assoc []));
  Alcotest.(check bool) "not object" false (Instance.is_type "object" (`Int 1))

let test_type_number () =
  Alcotest.(check bool) "int is number" true (Instance.is_type "number" (`Int 1));
  Alcotest.(check bool) "float is number" true (Instance.is_type "number" (`Float 1.5));
  Alcotest.(check bool) "intlit is number" true (Instance.is_type "number" (`Intlit "99"));
  Alcotest.(check bool) "bool not number" false (Instance.is_type "number" (`Bool true))

let test_type_integer () =
  Alcotest.(check bool) "int" true (Instance.is_type "integer" (`Int 1));
  Alcotest.(check bool) "float 1.0" true (Instance.is_type "integer" (`Float 1.0));
  Alcotest.(check bool) "float 1.1" false (Instance.is_type "integer" (`Float 1.1));
  Alcotest.(check bool) "intlit" true (Instance.is_type "integer" (`Intlit "42"));
  Alcotest.(check bool) "bool" false (Instance.is_type "integer" (`Bool true))

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "instance" [
    "json_equal", [
      Alcotest.test_case "null" `Quick test_equal_null;
      Alcotest.test_case "bool" `Quick test_equal_bool;
      Alcotest.test_case "string" `Quick test_equal_string;
      Alcotest.test_case "int" `Quick test_equal_int;
      Alcotest.test_case "float" `Quick test_equal_float;
      Alcotest.test_case "int=float" `Quick test_equal_int_float;
      Alcotest.test_case "bool<>number" `Quick test_equal_bool_vs_number;
      Alcotest.test_case "diff types" `Quick test_equal_diff_types;
      Alcotest.test_case "arrays" `Quick test_equal_arrays;
      Alcotest.test_case "objects" `Quick test_equal_objects;
      Alcotest.test_case "deep bool/number" `Quick test_equal_deep_bool_number;
      Alcotest.test_case "unicode no normalization" `Quick test_equal_unicode_no_normalization;
    ];
    "utf8_length", [
      Alcotest.test_case "ascii" `Quick test_utf8_ascii;
      Alcotest.test_case "empty" `Quick test_utf8_empty;
      Alcotest.test_case "2-byte" `Quick test_utf8_2byte;
      Alcotest.test_case "3-byte" `Quick test_utf8_3byte;
      Alcotest.test_case "4-byte" `Quick test_utf8_4byte;
      Alcotest.test_case "mixed" `Quick test_utf8_mixed;
      Alcotest.test_case "two emoji" `Quick test_utf8_two_emoji;
    ];
    "to_float", [
      Alcotest.test_case "int" `Quick test_to_float_int;
      Alcotest.test_case "float" `Quick test_to_float_float;
      Alcotest.test_case "intlit" `Quick test_to_float_intlit;
      Alcotest.test_case "non-number" `Quick test_to_float_non_number;
    ];
    "to_int", [
      Alcotest.test_case "int" `Quick test_to_int_int;
      Alcotest.test_case "float integer" `Quick test_to_int_float_integer;
      Alcotest.test_case "float non-integer" `Quick test_to_int_float_non_integer;
      Alcotest.test_case "non-number" `Quick test_to_int_non_number;
    ];
    "is_type", [
      Alcotest.test_case "null" `Quick test_type_null;
      Alcotest.test_case "boolean" `Quick test_type_boolean;
      Alcotest.test_case "string" `Quick test_type_string;
      Alcotest.test_case "array" `Quick test_type_array;
      Alcotest.test_case "object" `Quick test_type_object;
      Alcotest.test_case "number" `Quick test_type_number;
      Alcotest.test_case "integer" `Quick test_type_integer;
    ];
  ]
