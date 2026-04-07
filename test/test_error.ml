(* Behavioral tests for Error. *)

open Jsonschema_core

(* ══════════════════════════════════════════════════════════════ *)
(* make + field access                                           *)
(* ══════════════════════════════════════════════════════════════ *)

let test_make_fields () =
  let e = Error.make
    ~instance_path:["foo"; "0"]
    ~schema_path:["properties"; "foo"; "items"]
    ~keyword:"type"
    "expected integer, got string" in
  Alcotest.(check string) "keyword" "type" e.keyword;
  Alcotest.(check string) "message" "expected integer, got string" e.message;
  Alcotest.(check (list string)) "instance_path" ["foo"; "0"] e.instance_path;
  Alcotest.(check (list string)) "schema_path" ["properties"; "foo"; "items"] e.schema_path;
  Alcotest.(check int) "no context" 0 (List.length e.context)

let test_make_with_context () =
  let sub = Error.make
    ~instance_path:["x"] ~schema_path:["allOf"; "0"]
    ~keyword:"type" "wrong type" in
  let e = Error.make_with_context
    ~instance_path:["x"] ~schema_path:["allOf"]
    ~keyword:"allOf" ~context:[sub]
    "not valid against all schemas" in
  Alcotest.(check int) "has context" 1 (List.length e.context);
  Alcotest.(check string) "sub keyword" "type" (List.hd e.context).keyword

(* ══════════════════════════════════════════════════════════════ *)
(* to_string                                                     *)
(* ══════════════════════════════════════════════════════════════ *)

let test_to_string_contains_path () =
  let e = Error.make
    ~instance_path:["foo"] ~schema_path:["properties"; "foo"]
    ~keyword:"required" "missing property" in
  let s = Error.to_string e in
  Alcotest.(check bool) "has /foo" true (String.length s > 0);
  Alcotest.(check bool) "contains keyword" true
    (let re = Str.regexp_string "required" in
     try ignore (Str.search_forward re s 0); true with Not_found -> false)

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "error" [
    "make", [
      Alcotest.test_case "fields" `Quick test_make_fields;
      Alcotest.test_case "with context" `Quick test_make_with_context;
    ];
    "to_string", [
      Alcotest.test_case "contains path" `Quick test_to_string_contains_path;
    ];
  ]
