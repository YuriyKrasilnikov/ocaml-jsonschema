(** Keywords: allOf, anyOf, oneOf, not (spec §6.7). *)

open Jsonschema_core

let compile_all_of : Compiler.keyword_compiler = {
  keyword = "allOf";
  compile = fun ctx value ->
    match value with
    | `List schemas when schemas <> [] ->
      let sub_vs = List.map ctx.compile_sub schemas in
      Some (fun instance path ->
        let errors = List.concat_map (fun v -> v instance path) sub_vs in
        if errors = [] then []
        else [Error.make_with_context ~instance_path:path ~schema_path:ctx.schema_path
                ~keyword:"allOf" ~context:errors "not valid against all schemas"])
    | _ -> None;
}

let compile_any_of : Compiler.keyword_compiler = {
  keyword = "anyOf";
  compile = fun ctx value ->
    match value with
    | `List schemas when schemas <> [] ->
      let sub_vs = List.map ctx.compile_sub schemas in
      Some (fun instance path ->
        if List.exists (fun v -> v instance path = []) sub_vs then []
        else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                ~keyword:"anyOf" "not valid against any schema"])
    | _ -> None;
}

let compile_one_of : Compiler.keyword_compiler = {
  keyword = "oneOf";
  compile = fun ctx value ->
    match value with
    | `List schemas when schemas <> [] ->
      let sub_vs = List.map ctx.compile_sub schemas in
      Some (fun instance path ->
        let passing = List.filter (fun v -> v instance path = []) sub_vs in
        match List.length passing with
        | 1 -> []
        | 0 -> [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                   ~keyword:"oneOf" "not valid against any schema"]
        | _ -> [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                   ~keyword:"oneOf" "valid against more than one schema"])
    | _ -> None;
}

let compile_not : Compiler.keyword_compiler = {
  keyword = "not";
  compile = fun ctx value ->
    let sub_v = ctx.compile_sub value in
    Some (fun instance path ->
      if sub_v instance path <> [] then []
      else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
              ~keyword:"not" "should not be valid"]);
}

let compilers = [compile_all_of; compile_any_of; compile_one_of; compile_not]
