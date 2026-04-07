(* Behavioral tests for Compiler — schema compilation. *)

open Jsonschema_core

let j s = Yojson.Safe.from_string s
let validate schema_json instance =
  let reg = Registry.create () in
  let v = Compiler.compile Compiler.default_config reg schema_json in
  v instance Json_pointer.empty
let is_valid schema_json instance =
  validate schema_json instance = []

(* ══════════════════════════════════════════════════════════════ *)
(* Boolean schemas                                               *)
(* ══════════════════════════════════════════════════════════════ *)

(* true schema: everything validates *)
let test_bool_true () =
  Alcotest.(check bool) "null" true (is_valid (`Bool true) `Null);
  Alcotest.(check bool) "int" true (is_valid (`Bool true) (`Int 42));
  Alcotest.(check bool) "string" true (is_valid (`Bool true) (`String "x"));
  Alcotest.(check bool) "object" true (is_valid (`Bool true) (j "{}"));
  Alcotest.(check bool) "array" true (is_valid (`Bool true) (j "[]"))

(* false schema: nothing validates *)
let test_bool_false () =
  Alcotest.(check bool) "null" false (is_valid (`Bool false) `Null);
  Alcotest.(check bool) "int" false (is_valid (`Bool false) (`Int 42));
  Alcotest.(check bool) "string" false (is_valid (`Bool false) (`String "x"));
  Alcotest.(check bool) "object" false (is_valid (`Bool false) (j "{}"))

(* false schema: error has keyword "false" *)
let test_bool_false_error () =
  let errors = validate (`Bool false) `Null in
  Alcotest.(check int) "one error" 1 (List.length errors);
  Alcotest.(check string) "keyword" "false" (List.hd errors).keyword

(* ══════════════════════════════════════════════════════════════ *)
(* Empty schema = always valid (no keywords = no constraints)    *)
(* ══════════════════════════════════════════════════════════════ *)

let test_empty_schema () =
  Alcotest.(check bool) "null" true (is_valid (j "{}") `Null);
  Alcotest.(check bool) "int" true (is_valid (j "{}") (`Int 1));
  Alcotest.(check bool) "string" true (is_valid (j "{}") (`String "x"))

(* ══════════════════════════════════════════════════════════════ *)
(* Unknown keywords: ignored (spec: "SHOULD be ignored")         *)
(* ══════════════════════════════════════════════════════════════ *)

let test_unknown_keywords () =
  let schema = j {|{"x-custom": true, "foo": 42}|} in
  Alcotest.(check bool) "unknown ignored" true (is_valid schema (`Int 1))

(* ══════════════════════════════════════════════════════════════ *)
(* $ref resolution                                               *)
(* ══════════════════════════════════════════════════════════════ *)

(* $ref to local definition *)
let test_ref_local () =
  (* We need a keyword to actually validate something.
     Use a minimal "type" keyword compiler for testing. *)
  let type_kw : Compiler.keyword_compiler = {
    keyword = "type";
    compile = fun ctx value ->
      ignore ctx;
      (match value with
       | `String typ ->
         Some (fun instance path ->
           if Instance.is_type typ instance then []
           else [Error.make ~instance_path:path
                   ~schema_path:ctx.schema_path ~keyword:"type"
                   (Printf.sprintf "expected %s" typ)])
       | _ -> None);
  } in
  let config = { Compiler.keywords = [type_kw]; format_assertion = false } in
  let reg = Registry.create () in
  let schema = j {|{
    "definitions": { "str": { "type": "string" } },
    "$ref": "#/definitions/str"
  }|} in
  let base = Uri.of_string "http://test.com/s.json" in
  Registry.add reg base schema;
  let v = Compiler.compile config reg ~base_uri:base schema in
  Alcotest.(check bool) "string valid" true (v (`String "hello") [] = []);
  Alcotest.(check bool) "int invalid" false (v (`Int 1) [] = [])

(* $ref ignores sibling keywords *)
let test_ref_ignores_siblings () =
  let type_kw : Compiler.keyword_compiler = {
    keyword = "type";
    compile = fun ctx value ->
      ignore ctx;
      (match value with
       | `String typ ->
         Some (fun instance path ->
           if Instance.is_type typ instance then []
           else [Error.make ~instance_path:path
                   ~schema_path:ctx.schema_path ~keyword:"type"
                   (Printf.sprintf "expected %s" typ)])
       | _ -> None);
  } in
  let config = { Compiler.keywords = [type_kw]; format_assertion = false } in
  let reg = Registry.create () in
  (* $ref + type: "integer" — type should be IGNORED because $ref present *)
  let schema = j {|{
    "definitions": { "str": { "type": "string" } },
    "$ref": "#/definitions/str",
    "type": "integer"
  }|} in
  let base = Uri.of_string "http://test.com/s.json" in
  Registry.add reg base schema;
  let v = Compiler.compile config reg ~base_uri:base schema in
  (* String should be valid (matched by $ref target "type":"string") *)
  Alcotest.(check bool) "string valid via ref" true (v (`String "x") [] = []);
  (* Integer should be INvalid (type:"integer" sibling is ignored) *)
  Alcotest.(check bool) "int invalid" false (v (`Int 1) [] = [])

(* ══════════════════════════════════════════════════════════════ *)
(* Keyword dispatch: registered keywords produce validators      *)
(* ══════════════════════════════════════════════════════════════ *)

let test_keyword_dispatch () =
  let always_fail : Compiler.keyword_compiler = {
    keyword = "x-fail";
    compile = fun ctx _value ->
      Some (fun _instance path ->
        [Error.make ~instance_path:path ~schema_path:ctx.schema_path
           ~keyword:"x-fail" "always fails"]);
  } in
  let config = { Compiler.keywords = [always_fail]; format_assertion = false } in
  let reg = Registry.create () in
  let schema = j {|{"x-fail": true}|} in
  let v = Compiler.compile config reg schema in
  Alcotest.(check bool) "fails" false (v (`Int 1) [] = [])

(* Multiple keywords: all must pass *)
let test_multiple_keywords () =
  let pass_kw name : Compiler.keyword_compiler = {
    keyword = name;
    compile = fun _ctx _value -> Some (fun _inst _path -> []);
  } in
  let fail_kw name : Compiler.keyword_compiler = {
    keyword = name;
    compile = fun ctx _value ->
      Some (fun _inst path ->
        [Error.make ~instance_path:path ~schema_path:ctx.schema_path
           ~keyword:name "fail"]);
  } in
  let config = { Compiler.keywords = [pass_kw "a"; fail_kw "b"]; format_assertion = false } in
  let reg = Registry.create () in
  let schema = j {|{"a": 1, "b": 2}|} in
  let v = Compiler.compile config reg schema in
  let errors = v (`Int 1) [] in
  Alcotest.(check int) "one error from b" 1 (List.length errors);
  Alcotest.(check string) "keyword b" "b" (List.hd errors).keyword

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "compiler" [
    "boolean", [
      Alcotest.test_case "true" `Quick test_bool_true;
      Alcotest.test_case "false" `Quick test_bool_false;
      Alcotest.test_case "false error" `Quick test_bool_false_error;
    ];
    "empty", [
      Alcotest.test_case "empty schema" `Quick test_empty_schema;
    ];
    "unknown", [
      Alcotest.test_case "ignored" `Quick test_unknown_keywords;
    ];
    "$ref", [
      Alcotest.test_case "local" `Quick test_ref_local;
      Alcotest.test_case "ignores siblings" `Quick test_ref_ignores_siblings;
    ];
    "dispatch", [
      Alcotest.test_case "single keyword" `Quick test_keyword_dispatch;
      Alcotest.test_case "multiple keywords" `Quick test_multiple_keywords;
    ];
  ]
