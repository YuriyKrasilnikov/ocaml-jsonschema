(** Keywords: if, then, else (spec §6.6). *)

open Jsonschema_core

let compile_if : Compiler.keyword_compiler = {
  keyword = "if";
  compile = fun ctx value ->
    let if_v = ctx.compile_sub value in
    let then_v = match Schema.keyword "then" (Schema.Obj ctx.schema_obj) with
      | Some s -> Some (ctx.compile_sub s)
      | None -> None
    in
    let else_v = match Schema.keyword "else" (Schema.Obj ctx.schema_obj) with
      | Some s -> Some (ctx.compile_sub s)
      | None -> None
    in
    Some (fun instance path ->
      if if_v instance path = [] then
        (* if validates → apply then *)
        match then_v with
        | Some v -> v instance path
        | None -> []
      else
        (* if fails → apply else *)
        match else_v with
        | Some v -> v instance path
        | None -> []);
}

let compilers = [compile_if]
