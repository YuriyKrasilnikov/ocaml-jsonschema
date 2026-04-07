(** Keywords: maxLength, minLength, pattern (spec §6.3). *)

open Jsonschema_core

let compile_max_length : Compiler.keyword_compiler = {
  keyword = "maxLength";
  compile = fun ctx value ->
    match Util.to_nonneg_int value with
    | Some n ->
      Some (fun instance path ->
        match instance with
        | `String s when Instance.utf8_length s > n ->
          [Error.make ~instance_path:path ~schema_path:ctx.schema_path
             ~keyword:"maxLength" (Printf.sprintf "string too long (max %d)" n)]
        | _ -> [])
    | None -> None;
}

let compile_min_length : Compiler.keyword_compiler = {
  keyword = "minLength";
  compile = fun ctx value ->
    match Util.to_nonneg_int value with
    | Some n ->
      Some (fun instance path ->
        match instance with
        | `String s when Instance.utf8_length s < n ->
          [Error.make ~instance_path:path ~schema_path:ctx.schema_path
             ~keyword:"minLength" (Printf.sprintf "string too short (min %d)" n)]
        | _ -> [])
    | None -> None;
}

let compile_pattern : Compiler.keyword_compiler = {
  keyword = "pattern";
  compile = fun ctx value ->
    match value with
    | `String pat ->
      (match Regex.compile pat with
       | Ok re ->
         Some (fun instance path ->
           match instance with
           | `String s ->
             if Regex.matches re s then []
             else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                     ~keyword:"pattern" (Printf.sprintf "does not match pattern %s" pat)]
           | _ -> [])
       | Error _ -> None)
    | _ -> None;
}

let compilers = [compile_max_length; compile_min_length; compile_pattern]
