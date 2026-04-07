(** Keywords: type, enum, const (spec §6.1 — any instance type). *)

open Jsonschema_core

(* ── type ── *)

let compile_type : Compiler.keyword_compiler = {
  keyword = "type";
  compile = fun ctx value ->
    match value with
    | `String typ ->
      Some (fun instance path ->
        if Instance.is_type typ instance then []
        else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                ~keyword:"type" (Printf.sprintf "expected %s" typ)])
    | `List types ->
      let type_strings = List.filter_map (function
        | `String s -> Some s | _ -> None) types in
      Some (fun instance path ->
        if List.exists (fun typ -> Instance.is_type typ instance) type_strings then []
        else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                ~keyword:"type" "type mismatch"])
    | _ -> None;
}

(* ── enum ── *)

let compile_enum : Compiler.keyword_compiler = {
  keyword = "enum";
  compile = fun ctx value ->
    match value with
    | `List values ->
      Some (fun instance path ->
        if List.exists (fun v -> Instance.json_equal v instance) values then []
        else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                ~keyword:"enum" "value not in enum"])
    | _ -> None;
}

(* ── const ── *)

let compile_const : Compiler.keyword_compiler = {
  keyword = "const";
  compile = fun ctx value ->
    Some (fun instance path ->
      if Instance.json_equal value instance then []
      else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
              ~keyword:"const" "value does not match const"]);
}

let compilers = [compile_type; compile_enum; compile_const]
