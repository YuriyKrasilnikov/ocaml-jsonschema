(** Schema compiler — transforms JSON Schema into validator functions. *)

type config = {
  keywords : keyword_compiler list;
  format_assertion : bool;
}

and keyword_compiler = {
  keyword : string;
  compile : context -> Yojson.Safe.t -> Registry.validator option;
}

and context = {
  config : config;
  registry : Registry.t;
  base_uri : Uri.t;
  schema_path : Json_pointer.t;
  schema_obj : Yojson.Safe.t;
  compile_sub : Yojson.Safe.t -> Registry.validator;
}

let default_config = { keywords = []; format_assertion = false }

let rec compile config reg ?(base_uri = Uri.empty) schema_json =
  match schema_json with
  | `Bool true -> fun _instance _path -> []
  | `Bool false ->
    fun _instance path ->
      [Error.make ~instance_path:path ~schema_path:[]
         ~keyword:"false" "false schema always fails"]
  | `Assoc pairs -> compile_obj config reg base_uri pairs schema_json
  | _ -> fun _instance _path -> []

and compile_obj config reg base_uri pairs schema_json =
  (* $ref ignores all siblings including $id (draft-07 §8.3). *)
  match List.assoc_opt "$ref" pairs with
  | Some (`String ref_str) ->
    (* $ref present — resolve against current base_uri, ignore sibling $id *)
    compile_ref config reg base_uri ref_str
  | _ ->
    (* No $ref — process $id to update base URI *)
    let base_uri =
      match List.assoc_opt "$id" pairs with
      | Some (`String id_str) ->
        let id_uri = Uri.of_string id_str in
        let resolved = Uri_util.resolve ~base:base_uri id_uri in
        if Uri_util.fragment id_uri <> None && Uri.path id_uri = "" then
          base_uri
        else
          Uri_util.without_fragment resolved
      | _ -> base_uri
    in
    compile_keywords config reg base_uri pairs schema_json

and compile_ref config reg base_uri ref_str =
  let ref_uri = Uri.of_string ref_str in
  let resolved = Uri_util.resolve ~base:base_uri ref_uri in
  match Registry.get_compiled reg resolved with
  | Some v -> v
  | None ->
    if Registry.is_compiling reg resolved then begin
      let proxy = ref (fun (_inst : Yojson.Safe.t) (_path : Json_pointer.t) -> []) in
      Registry.on_compiled reg resolved (fun v -> proxy := v);
      fun instance path -> !proxy instance path
    end else begin
      (* resolve_with_base tracks $id along JSON Pointer path *)
      match Registry.resolve_with_base reg resolved with
      | Some (target_json, target_base) ->
        Registry.mark_compiling reg resolved;
        let v = compile config reg ~base_uri:target_base target_json in
        Registry.set_compiled reg resolved v;
        v
      | None ->
        fun _instance path ->
          [Error.make ~instance_path:path ~schema_path:[]
             ~keyword:"$ref" (Printf.sprintf "unresolved $ref: %s" ref_str)]
    end

and compile_keywords config reg base_uri pairs schema_json =
  let compile_sub sub_json =
    compile config reg ~base_uri sub_json
  in
  let ctx = {
    config;
    registry = reg;
    base_uri;
    schema_path = [];
    schema_obj = schema_json;
    compile_sub;
  } in
  let validators = List.filter_map (fun (key, value) ->
    List.find_opt (fun kc -> kc.keyword = key) config.keywords
    |> Option.map (fun kc ->
      let ctx = { ctx with schema_path = Json_pointer.append ctx.schema_path key } in
      kc.compile ctx value)
    |> Option.join
  ) pairs in
  match validators with
  | [] -> fun _instance _path -> []
  | validators ->
    fun instance path ->
      List.concat_map (fun v -> v instance path) validators
