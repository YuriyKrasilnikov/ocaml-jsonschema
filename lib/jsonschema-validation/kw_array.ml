(** Keywords: items, additionalItems, maxItems, minItems, uniqueItems, contains (spec §6.4). *)

open Jsonschema_core

let compile_items : Compiler.keyword_compiler = {
  keyword = "items";
  compile = fun ctx value ->
    match value with
    | `Assoc _ | `Bool _ ->
      (* items is a single schema — applies to all elements *)
      let sub_v = ctx.compile_sub value in
      Some (fun instance path ->
        match instance with
        | `List items ->
          List.concat_map (fun (i, item) ->
            let p = Json_pointer.append path (string_of_int i) in
            sub_v item p
          ) (List.mapi (fun i x -> (i, x)) items)
        | _ -> [])
    | `List schemas ->
      (* items is tuple — each schema applies to corresponding element *)
      let sub_vs = List.map ctx.compile_sub schemas in
      (* Also check additionalItems *)
      let additional_v =
        match Schema.keyword "additionalItems" (Schema.Obj ctx.schema_obj) with
        | Some (`Bool false) ->
          Some (fun _instance path i ->
            [Error.make ~instance_path:(Json_pointer.append path (string_of_int i))
               ~schema_path:ctx.schema_path ~keyword:"additionalItems"
               "additional items not allowed"])
        | Some (`Bool true) | None -> None
        | Some additional_schema ->
          let v = ctx.compile_sub additional_schema in
          Some (fun instance path i ->
            v instance (Json_pointer.append path (string_of_int i)))
      in
      Some (fun instance path ->
        match instance with
        | `List items ->
          let indexed = List.mapi (fun i x -> (i, x)) items in
          let tuple_errors = List.concat_map (fun (i, item) ->
            if i < List.length sub_vs then
              let p = Json_pointer.append path (string_of_int i) in
              (List.nth sub_vs i) item p
            else []
          ) indexed in
          let additional_errors = match additional_v with
            | None -> []
            | Some check ->
              List.concat_map (fun (i, item) ->
                if i >= List.length sub_vs then check item path i
                else []
              ) indexed
          in
          tuple_errors @ additional_errors
        | _ -> [])
    | _ -> None;
}

let compile_additional_items : Compiler.keyword_compiler = {
  keyword = "additionalItems";
  compile = fun ctx _value ->
    (* Only meaningful when items is an array — handled by items compiler *)
    match Schema.keyword "items" (Schema.Obj ctx.schema_obj) with
    | Some (`List _) -> None  (* handled by items *)
    | _ -> None;  (* ignored when items is schema or absent *)
}

let compile_max_items : Compiler.keyword_compiler = {
  keyword = "maxItems";
  compile = fun ctx value ->
    match Util.to_nonneg_int value with
    | Some n ->
      Some (fun instance path ->
        match instance with
        | `List items when List.length items > n ->
          [Error.make ~instance_path:path ~schema_path:ctx.schema_path
             ~keyword:"maxItems" (Printf.sprintf "too many items (max %d)" n)]
        | _ -> [])
    | None -> None;
}

let compile_min_items : Compiler.keyword_compiler = {
  keyword = "minItems";
  compile = fun ctx value ->
    match Util.to_nonneg_int value with
    | Some n ->
      Some (fun instance path ->
        match instance with
        | `List items when List.length items < n ->
          [Error.make ~instance_path:path ~schema_path:ctx.schema_path
             ~keyword:"minItems" (Printf.sprintf "too few items (min %d)" n)]
        | _ -> [])
    | None -> None;
}

let compile_unique_items : Compiler.keyword_compiler = {
  keyword = "uniqueItems";
  compile = fun ctx value ->
    match value with
    | `Bool true ->
      Some (fun instance path ->
        match instance with
        | `List items ->
          let rec has_dup = function
            | [] -> false
            | x :: rest ->
              if List.exists (Instance.json_equal x) rest then true
              else has_dup rest
          in
          if has_dup items then
            [Error.make ~instance_path:path ~schema_path:ctx.schema_path
               ~keyword:"uniqueItems" "items are not unique"]
          else []
        | _ -> [])
    | _ -> None;
}

let compile_contains : Compiler.keyword_compiler = {
  keyword = "contains";
  compile = fun ctx value ->
    let sub_v = ctx.compile_sub value in
    Some (fun instance path ->
      match instance with
      | `List items ->
        if List.exists (fun item -> sub_v item path = []) items then []
        else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                ~keyword:"contains" "no item matches contains schema"]
      | _ -> []);
}

let compilers = [
  compile_items; compile_additional_items;
  compile_max_items; compile_min_items;
  compile_unique_items; compile_contains;
]
