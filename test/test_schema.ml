(* Behavioral tests for Schema — JSON Schema representation. *)

open Jsonschema_core

let j s = Yojson.Safe.from_string s

(* ══════════════════════════════════════════════════════════════ *)
(* of_json                                                       *)
(* ══════════════════════════════════════════════════════════════ *)

(* Happy path: boolean schemas *)
let test_of_json_true () =
  match Schema.of_json (`Bool true) with
  | Ok (Schema.Bool true) -> ()
  | _ -> Alcotest.fail "expected Bool true"

let test_of_json_false () =
  match Schema.of_json (`Bool false) with
  | Ok (Schema.Bool false) -> ()
  | _ -> Alcotest.fail "expected Bool false"

(* Happy path: object schema *)
let test_of_json_object () =
  match Schema.of_json (j {|{"type": "string"}|}) with
  | Ok (Schema.Obj _) -> ()
  | _ -> Alcotest.fail "expected Obj"

(* Happy path: empty object is valid schema *)
let test_of_json_empty_object () =
  match Schema.of_json (j "{}") with
  | Ok (Schema.Obj _) -> ()
  | _ -> Alcotest.fail "expected Obj for {}"

(* Negative: non-object, non-boolean *)
let test_of_json_string () =
  match Schema.of_json (`String "not a schema") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for string"

let test_of_json_number () =
  match Schema.of_json (`Int 42) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for number"

let test_of_json_array () =
  match Schema.of_json (j "[1,2,3]") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for array"

let test_of_json_null () =
  match Schema.of_json `Null with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for null"

(* ══════════════════════════════════════════════════════════════ *)
(* keyword                                                       *)
(* ══════════════════════════════════════════════════════════════ *)

let test_keyword_present () =
  let s = j {|{"type": "string", "maxLength": 10}|} in
  match Schema.of_json s with
  | Ok schema ->
    Alcotest.(check (option string)) "type"
      (Some "string")
      (match Schema.keyword "type" schema with
       | Some (`String s) -> Some s
       | _ -> None);
    Alcotest.(check (option int)) "maxLength"
      (Some 10)
      (match Schema.keyword "maxLength" schema with
       | Some (`Int n) -> Some n
       | _ -> None)
  | Error e -> Alcotest.fail e

let test_keyword_absent () =
  match Schema.of_json (j {|{"type": "string"}|}) with
  | Ok schema ->
    Alcotest.(check bool) "absent" true
      (Option.is_none (Schema.keyword "minLength" schema))
  | Error e -> Alcotest.fail e

let test_keyword_bool_schema () =
  match Schema.of_json (`Bool true) with
  | Ok schema ->
    Alcotest.(check bool) "no keywords" true
      (Option.is_none (Schema.keyword "type" schema))
  | Error e -> Alcotest.fail e

(* ══════════════════════════════════════════════════════════════ *)
(* keywords                                                      *)
(* ══════════════════════════════════════════════════════════════ *)

let test_keywords_list () =
  match Schema.of_json (j {|{"type": "object", "required": ["a"]}|}) with
  | Ok schema ->
    let kws = Schema.keywords schema |> List.sort String.compare in
    Alcotest.(check (list string)) "keywords"
      ["required"; "type"] kws
  | Error e -> Alcotest.fail e

let test_keywords_bool () =
  match Schema.of_json (`Bool false) with
  | Ok schema ->
    Alcotest.(check (list string)) "empty" [] (Schema.keywords schema)
  | Error e -> Alcotest.fail e

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "schema" [
    "of_json", [
      Alcotest.test_case "true" `Quick test_of_json_true;
      Alcotest.test_case "false" `Quick test_of_json_false;
      Alcotest.test_case "object" `Quick test_of_json_object;
      Alcotest.test_case "empty object" `Quick test_of_json_empty_object;
      Alcotest.test_case "string rejected" `Quick test_of_json_string;
      Alcotest.test_case "number rejected" `Quick test_of_json_number;
      Alcotest.test_case "array rejected" `Quick test_of_json_array;
      Alcotest.test_case "null rejected" `Quick test_of_json_null;
    ];
    "keyword", [
      Alcotest.test_case "present" `Quick test_keyword_present;
      Alcotest.test_case "absent" `Quick test_keyword_absent;
      Alcotest.test_case "bool schema" `Quick test_keyword_bool_schema;
    ];
    "keywords", [
      Alcotest.test_case "list" `Quick test_keywords_list;
      Alcotest.test_case "bool schema" `Quick test_keywords_bool;
    ];
  ]
