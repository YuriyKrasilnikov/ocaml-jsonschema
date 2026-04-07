(* Behavioral tests for Registry — schema storage and resolution. *)

open Jsonschema_core

let j s = Yojson.Safe.from_string s

let json_t = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal
let opt_json = Alcotest.option json_t

(* ══════════════════════════════════════════════════════════════ *)
(* add + resolve_raw                                             *)
(* ══════════════════════════════════════════════════════════════ *)

(* Happy path: add a schema, resolve it back *)
let test_add_resolve () =
  let reg = Registry.create () in
  let uri = Uri.of_string "http://example.com/schema.json" in
  let schema = j {|{"type": "string"}|} in
  Registry.add reg uri schema;
  Alcotest.(check opt_json) "found" (Some schema) (Registry.resolve_raw reg uri)

(* Boundary: resolve unknown URI → None *)
let test_resolve_unknown () =
  let reg = Registry.create () in
  let uri = Uri.of_string "http://example.com/nope.json" in
  Alcotest.(check opt_json) "not found" None (Registry.resolve_raw reg uri)

(* Happy path: $id in root registers the schema under that URI *)
let test_id_root () =
  let reg = Registry.create () in
  let base = Uri.of_string "http://example.com/base.json" in
  let schema = j {|{"$id": "http://example.com/root.json", "type": "integer"}|} in
  Registry.add reg base schema;
  Alcotest.(check opt_json) "by $id"
    (Some schema)
    (Registry.resolve_raw reg (Uri.of_string "http://example.com/root.json"))

(* Happy path: $id in definitions registers sub-schemas *)
let test_id_in_definitions () =
  let reg = Registry.create () in
  let base = Uri.of_string "http://example.com/root.json" in
  let schema = j {|{
    "$id": "http://example.com/root.json",
    "definitions": {
      "str": {
        "$id": "http://example.com/str.json",
        "type": "string"
      }
    }
  }|} in
  Registry.add reg base schema;
  let str_schema = j {|{"$id": "http://example.com/str.json", "type": "string"}|} in
  Alcotest.(check opt_json) "sub-schema by $id"
    (Some str_schema)
    (Registry.resolve_raw reg (Uri.of_string "http://example.com/str.json"))

(* Happy path: $id with fragment creates named anchor *)
let test_id_fragment () =
  let reg = Registry.create () in
  let base = Uri.of_string "http://example.com/root.json" in
  let schema = j {|{
    "$id": "http://example.com/root.json",
    "definitions": {
      "A": { "$id": "#foo", "type": "integer" }
    }
  }|} in
  Registry.add reg base schema;
  let a_schema = j {|{"$id": "#foo", "type": "integer"}|} in
  Alcotest.(check opt_json) "anchor"
    (Some a_schema)
    (Registry.resolve_raw reg (Uri.of_string "http://example.com/root.json#foo"))

(* Happy path: JSON Pointer fragment resolves within document *)
let test_json_pointer_fragment () =
  let reg = Registry.create () in
  let base = Uri.of_string "http://example.com/root.json" in
  let schema = j {|{
    "definitions": {
      "pos": { "type": "integer", "minimum": 0 }
    }
  }|} in
  Registry.add reg base schema;
  let pos = j {|{"type": "integer", "minimum": 0}|} in
  Alcotest.(check opt_json) "json pointer"
    (Some pos)
    (Registry.resolve_raw reg (Uri.of_string "http://example.com/root.json#/definitions/pos"))

(* ══════════════════════════════════════════════════════════════ *)
(* retriever                                                     *)
(* ══════════════════════════════════════════════════════════════ *)

(* Happy path: retriever called for unknown URIs *)
let test_retriever () =
  let remote = j {|{"type": "number"}|} in
  let retriever uri =
    if Uri.to_string uri = "http://remote.com/num.json" then Some remote
    else None
  in
  let reg = Registry.create ~retriever () in
  Alcotest.(check opt_json) "retrieved"
    (Some remote)
    (Registry.resolve_raw reg (Uri.of_string "http://remote.com/num.json"))

(* Boundary: retriever not called if registry has it *)
let test_retriever_not_called_if_local () =
  let called = ref false in
  let retriever _uri = called := true; None in
  let reg = Registry.create ~retriever () in
  let uri = Uri.of_string "http://local.com/s.json" in
  let schema = j {|{"type": "boolean"}|} in
  Registry.add reg uri schema;
  ignore (Registry.resolve_raw reg uri);
  Alcotest.(check bool) "not called" false !called

(* ══════════════════════════════════════════════════════════════ *)
(* Compilation state: mark_compiling, is_compiling, set_compiled *)
(* ══════════════════════════════════════════════════════════════ *)

let test_compilation_lifecycle () =
  let reg = Registry.create () in
  let uri = Uri.of_string "http://example.com/s.json" in
  Alcotest.(check bool) "not compiling initially" false (Registry.is_compiling reg uri);
  Registry.mark_compiling reg uri;
  Alcotest.(check bool) "compiling after mark" true (Registry.is_compiling reg uri);
  let v _json _path = [] in
  Registry.set_compiled reg uri v;
  Alcotest.(check bool) "not compiling after set" false (Registry.is_compiling reg uri);
  Alcotest.(check bool) "has compiled" true (Option.is_some (Registry.get_compiled reg uri))

(* Circular ref: on_compiled callback fires when compilation completes *)
let test_on_compiled_callback () =
  let reg = Registry.create () in
  let uri = Uri.of_string "http://example.com/s.json" in
  Registry.mark_compiling reg uri;
  let received = ref false in
  Registry.on_compiled reg uri (fun _v -> received := true);
  Alcotest.(check bool) "not yet" false !received;
  let v _json _path = [] in
  Registry.set_compiled reg uri v;
  Alcotest.(check bool) "callback fired" true !received

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "registry" [
    "add+resolve", [
      Alcotest.test_case "basic" `Quick test_add_resolve;
      Alcotest.test_case "unknown" `Quick test_resolve_unknown;
      Alcotest.test_case "$id root" `Quick test_id_root;
      Alcotest.test_case "$id definitions" `Quick test_id_in_definitions;
      Alcotest.test_case "$id fragment" `Quick test_id_fragment;
      Alcotest.test_case "JSON Pointer" `Quick test_json_pointer_fragment;
    ];
    "retriever", [
      Alcotest.test_case "remote" `Quick test_retriever;
      Alcotest.test_case "local priority" `Quick test_retriever_not_called_if_local;
    ];
    "compilation", [
      Alcotest.test_case "lifecycle" `Quick test_compilation_lifecycle;
      Alcotest.test_case "on_compiled" `Quick test_on_compiled_callback;
    ];
  ]
