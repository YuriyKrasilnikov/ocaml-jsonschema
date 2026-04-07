(* JSON-Schema-Test-Suite runner for draft-07.
   Covers mandatory (922), optional (118), and format (535) tests. *)

let test_suite_dir = "test/suite/tests/draft7"
let remotes_dir = "test/suite/remotes"
let metaschema_path = "test/spec/schema.json"

(* Load all JSON files from remotes/ into a retriever function *)
let load_remotes () =
  let tbl = Hashtbl.create 64 in
  let rec scan dir prefix =
    let entries = Sys.readdir dir in
    Array.iter (fun entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then
        scan path (prefix ^ entry ^ "/")
      else if Filename.check_suffix entry ".json" then begin
        let uri = "http://localhost:1234/" ^ prefix ^ entry in
        let json = Yojson.Safe.from_file path in
        Hashtbl.replace tbl uri json
      end
    ) entries
  in
  scan remotes_dir "";
  (try
     let meta = Yojson.Safe.from_file metaschema_path in
     Hashtbl.replace tbl "http://json-schema.org/draft-07/schema" meta
   with _ -> ());
  fun uri ->
    let key = Uri.to_string uri in
    let key = if String.length key > 0 && key.[String.length key - 1] = '#'
      then String.sub key 0 (String.length key - 1)
      else key in
    Hashtbl.find_opt tbl key

type test_case = { description : string; data : Yojson.Safe.t; valid : bool }
type test_group = { group_description : string; schema : Yojson.Safe.t; tests : test_case list }

let parse_test_file path : test_group list =
  let json = Yojson.Safe.from_file path in
  match json with
  | `List groups ->
    List.filter_map (fun group ->
      match group with
      | `Assoc pairs ->
        let desc = match List.assoc_opt "description" pairs with
          | Some (`String s) -> s | _ -> "unknown" in
        let schema = match List.assoc_opt "schema" pairs with
          | Some s -> s | _ -> `Null in
        let tests = match List.assoc_opt "tests" pairs with
          | Some (`List ts) ->
            List.filter_map (fun t ->
              match t with
              | `Assoc tp ->
                let d = match List.assoc_opt "description" tp with
                  | Some (`String s) -> s | _ -> "" in
                let data = match List.assoc_opt "data" tp with
                  | Some v -> v | _ -> `Null in
                let valid = match List.assoc_opt "valid" tp with
                  | Some (`Bool b) -> b | _ -> true in
                Some { description = d; data; valid }
              | _ -> None) ts
          | _ -> [] in
        Some { group_description = desc; schema; tests }
      | _ -> None) groups
  | _ -> []

let run_test_file ~format_assertion retriever path =
  let groups = parse_test_file path in
  let pass = ref 0 in
  let fail = ref 0 in
  let errors = ref [] in
  List.iter (fun group ->
    let validator =
      try Jsonschema_validation.compile ~format_assertion ~retriever group.schema
      with exn ->
        errors := (Printf.sprintf "%s: compile error: %s"
                     group.group_description (Printexc.to_string exn)) :: !errors;
        fun _inst _path -> []
    in
    List.iter (fun tc ->
      let actual_valid = Jsonschema_validation.is_valid validator tc.data in
      if actual_valid = tc.valid then incr pass
      else begin
        incr fail;
        errors := (Printf.sprintf "%s / %s: expected %s got %s"
                     group.group_description tc.description
                     (if tc.valid then "valid" else "invalid")
                     (if actual_valid then "valid" else "invalid")) :: !errors
      end
    ) group.tests
  ) groups;
  (!pass, !fail, List.rev !errors)

let run_category ~label ~format_assertion ~dir ~files retriever =
  let total_pass = ref 0 in
  let total_fail = ref 0 in
  let file_results = List.map (fun file ->
    let path = Filename.concat dir file in
    let pass, fail, errors = run_test_file ~format_assertion retriever path in
    total_pass := !total_pass + pass;
    total_fail := !total_fail + fail;
    (file, pass, fail, errors)
  ) files in
  Printf.printf "\n══ %s ══\n" label;
  List.iter (fun (file, pass, fail, _) ->
    if fail > 0 then
      Printf.printf "  %-40s %3d pass  %3d FAIL\n" file pass fail
    else
      Printf.printf "  %-40s %3d pass\n" file pass
  ) file_results;
  Printf.printf "  %s total: %d pass, %d fail\n" label !total_pass !total_fail;
  let all_errors = List.concat_map (fun (_, _, _, e) -> e) file_results in
  if all_errors <> [] then begin
    Printf.printf "  First failures:\n";
    List.iteri (fun i e -> if i < 100 then Printf.printf "    %s\n" e) all_errors
  end;
  (!total_pass, !total_fail)

let mandatory_files = [
  "additionalItems.json"; "additionalProperties.json"; "allOf.json";
  "anyOf.json"; "boolean_schema.json"; "const.json"; "contains.json";
  "default.json"; "definitions.json"; "dependencies.json"; "enum.json";
  "exclusiveMaximum.json"; "exclusiveMinimum.json"; "format.json";
  "if-then-else.json"; "infinite-loop-detection.json"; "items.json";
  "maxItems.json"; "maxLength.json"; "maxProperties.json"; "maximum.json";
  "minItems.json"; "minLength.json"; "minProperties.json"; "minimum.json";
  "multipleOf.json"; "not.json"; "oneOf.json"; "pattern.json";
  "patternProperties.json"; "properties.json"; "propertyNames.json";
  "ref.json"; "refRemote.json"; "required.json"; "type.json";
  "uniqueItems.json";
]

let optional_files = [
  "bignum.json"; "content.json"; "cross-draft.json";
  "ecmascript-regex.json"; "float-overflow.json"; "id.json";
  "non-bmp-regex.json"; "unknownKeyword.json";
]

let format_files = [
  "date-time.json"; "date.json"; "email.json"; "hostname.json";
  "idn-email.json"; "idn-hostname.json"; "ipv4.json"; "ipv6.json";
  "iri-reference.json"; "iri.json"; "json-pointer.json"; "regex.json";
  "relative-json-pointer.json"; "time.json"; "unknown.json";
  "uri-reference.json"; "uri-template.json"; "uri.json";
]

let () =
  let retriever = load_remotes () in
  let m_pass, m_fail = run_category ~label:"Mandatory"
    ~format_assertion:false ~dir:test_suite_dir ~files:mandatory_files retriever in
  let o_pass, o_fail = run_category ~label:"Optional"
    ~format_assertion:false
    ~dir:(Filename.concat test_suite_dir "optional") ~files:optional_files retriever in
  let f_pass, f_fail = run_category ~label:"Format"
    ~format_assertion:true
    ~dir:(Filename.concat test_suite_dir "optional/format") ~files:format_files retriever in
  let grand_pass = m_pass + o_pass + f_pass in
  let grand_fail = m_fail + o_fail + f_fail in
  Printf.printf "\n══ GRAND TOTAL: %d pass, %d fail (of %d) ══\n"
    grand_pass grand_fail (grand_pass + grand_fail);
  Alcotest.run "suite" [
    "mandatory", [
      Alcotest.test_case "922/922" `Quick (fun () ->
        if m_fail > 0 then
          Alcotest.fail (Printf.sprintf "%d mandatory tests fail" m_fail))
    ];
    "optional", [
      Alcotest.test_case "optional tests" `Quick (fun () ->
        Printf.printf "  Optional: %d/%d\n" o_pass (o_pass + o_fail))
    ];
    "format", [
      Alcotest.test_case "format tests" `Quick (fun () ->
        Printf.printf "  Format: %d/%d\n" f_pass (f_pass + f_fail))
    ];
  ]
