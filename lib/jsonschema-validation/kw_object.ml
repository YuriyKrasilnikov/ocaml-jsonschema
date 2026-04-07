(** Keywords: properties, patternProperties, additionalProperties,
    maxProperties, minProperties, required, dependencies, propertyNames (spec §6.5). *)

open Jsonschema_core

let compile_properties : Compiler.keyword_compiler = {
  keyword = "properties";
  compile = fun ctx value ->
    match value with
    | `Assoc prop_schemas ->
      let compiled = List.map (fun (name, schema) ->
        (name, ctx.compile_sub schema)
      ) prop_schemas in
      Some (fun instance path ->
        match instance with
        | `Assoc pairs ->
          List.concat_map (fun (name, v) ->
            match List.assoc_opt name pairs with
            | Some value ->
              v value (Json_pointer.append path name)
            | None -> []
          ) compiled
        | _ -> [])
    | _ -> None;
}

let compile_pattern_properties : Compiler.keyword_compiler = {
  keyword = "patternProperties";
  compile = fun ctx value ->
    match value with
    | `Assoc pat_schemas ->
      let compiled = List.filter_map (fun (pattern, schema) ->
        match Regex.compile pattern with
        | Ok re -> Some (re, ctx.compile_sub schema)
        | Error _ -> None
      ) pat_schemas in
      Some (fun instance path ->
        match instance with
        | `Assoc pairs ->
          List.concat_map (fun (name, value) ->
            List.concat_map (fun (re, v) ->
              if Regex.matches re name then
                v value (Json_pointer.append path name)
              else []
            ) compiled
          ) pairs
        | _ -> [])
    | _ -> None;
}

let compile_additional_properties : Compiler.keyword_compiler = {
  keyword = "additionalProperties";
  compile = fun ctx value ->
    let sub_v = ctx.compile_sub value in
    (* Collect property names from "properties" *)
    let prop_names = match Schema.keyword "properties" (Schema.Obj ctx.schema_obj) with
      | Some (`Assoc pairs) -> List.map fst pairs
      | _ -> []
    in
    (* Collect patterns from "patternProperties" *)
    let patterns = match Schema.keyword "patternProperties" (Schema.Obj ctx.schema_obj) with
      | Some (`Assoc pairs) ->
        List.filter_map (fun (pat, _) ->
          match Regex.compile pat with Ok re -> Some re | Error _ -> None
        ) pairs
      | _ -> []
    in
    Some (fun instance path ->
      match instance with
      | `Assoc pairs ->
        List.concat_map (fun (name, v) ->
          let matched_by_props = List.mem name prop_names in
          let matched_by_patterns = List.exists (fun re -> Regex.matches re name) patterns in
          if matched_by_props || matched_by_patterns then []
          else sub_v v (Json_pointer.append path name)
        ) pairs
      | _ -> []);
}

let compile_max_properties : Compiler.keyword_compiler = {
  keyword = "maxProperties";
  compile = fun ctx value ->
    match Util.to_nonneg_int value with
    | Some n ->
      Some (fun instance path ->
        match instance with
        | `Assoc pairs when List.length pairs > n ->
          [Error.make ~instance_path:path ~schema_path:ctx.schema_path
             ~keyword:"maxProperties" "too many properties"]
        | _ -> [])
    | None -> None;
}

let compile_min_properties : Compiler.keyword_compiler = {
  keyword = "minProperties";
  compile = fun ctx value ->
    match Util.to_nonneg_int value with
    | Some n ->
      Some (fun instance path ->
        match instance with
        | `Assoc pairs when List.length pairs < n ->
          [Error.make ~instance_path:path ~schema_path:ctx.schema_path
             ~keyword:"minProperties" "too few properties"]
        | _ -> [])
    | None -> None;
}

let compile_required : Compiler.keyword_compiler = {
  keyword = "required";
  compile = fun ctx value ->
    match value with
    | `List names ->
      let required = List.filter_map (function
        | `String s -> Some s | _ -> None) names in
      Some (fun instance path ->
        match instance with
        | `Assoc pairs ->
          List.concat_map (fun name ->
            if List.mem_assoc name pairs then []
            else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                    ~keyword:"required" (Printf.sprintf "missing required property: %s" name)]
          ) required
        | _ -> [])
    | _ -> None;
}

let compile_dependencies : Compiler.keyword_compiler = {
  keyword = "dependencies";
  compile = fun ctx value ->
    match value with
    | `Assoc deps ->
      let compiled = List.map (fun (prop, dep) ->
        match dep with
        | `List required_props ->
          (* Property dependency *)
          let names = List.filter_map (function
            | `String s -> Some s | _ -> None) required_props in
          (prop, `Props names)
        | _ ->
          (* Schema dependency *)
          (prop, `Schema (ctx.compile_sub dep))
      ) deps in
      Some (fun instance path ->
        match instance with
        | `Assoc pairs ->
          List.concat_map (fun (prop, dep) ->
            if not (List.mem_assoc prop pairs) then []
            else match dep with
            | `Props required ->
              List.concat_map (fun name ->
                if List.mem_assoc name pairs then []
                else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                        ~keyword:"dependencies"
                        (Printf.sprintf "%s requires %s" prop name)]
              ) required
            | `Schema v -> v instance path
          ) compiled
        | _ -> [])
    | _ -> None;
}

let compile_property_names : Compiler.keyword_compiler = {
  keyword = "propertyNames";
  compile = fun ctx value ->
    let sub_v = ctx.compile_sub value in
    Some (fun instance path ->
      match instance with
      | `Assoc pairs ->
        List.concat_map (fun (name, _) ->
          sub_v (`String name) (Json_pointer.append path name)
        ) pairs
      | _ -> []);
}

let compile_dependent_required : Compiler.keyword_compiler = {
  keyword = "dependentRequired";
  compile = fun ctx value ->
    match value with
    | `Assoc deps ->
      let compiled = List.filter_map (fun (prop, dep) ->
        match dep with
        | `List required_props ->
          let names = List.filter_map (function
            | `String s -> Some s | _ -> None) required_props in
          Some (prop, names)
        | _ -> None
      ) deps in
      Some (fun instance path ->
        match instance with
        | `Assoc pairs ->
          List.concat_map (fun (prop, required) ->
            if not (List.mem_assoc prop pairs) then []
            else
              List.concat_map (fun name ->
                if List.mem_assoc name pairs then []
                else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                        ~keyword:"dependentRequired"
                        (Printf.sprintf "%s requires %s" prop name)]
              ) required
          ) compiled
        | _ -> [])
    | _ -> None;
}

let compilers = [
  compile_properties; compile_pattern_properties;
  compile_additional_properties; compile_max_properties;
  compile_min_properties; compile_required;
  compile_dependencies; compile_property_names;
  compile_dependent_required;
]
