(** Keywords: multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum (spec §6.2). *)

open Jsonschema_core

let get_number j = Instance.to_float j

let compile_multiple_of : Compiler.keyword_compiler = {
  keyword = "multipleOf";
  compile = fun ctx value ->
    match get_number value with
    | Some divisor when divisor > 0.0 ->
      Some (fun instance path ->
        match get_number instance with
        | Some v ->
          if v = 0.0 then []
          else
            let r = v /. divisor in
            (match Float.classify_float r with
             | FP_infinite ->
               (* Overflow: if value is integer and divisor divides 1.0, it's valid *)
               if Float.is_integer divisor && mod_float v divisor = 0.0 then []
               else if mod_float (1.0 /. divisor) 1.0 = 0.0 && Float.is_integer v then []
               else [Error.make ~instance_path:path
                 ~schema_path:ctx.schema_path ~keyword:"multipleOf" "not a multiple"]
             | FP_nan -> [Error.make ~instance_path:path
                 ~schema_path:ctx.schema_path ~keyword:"multipleOf" "not a multiple"]
             | _ ->
               if Float.abs (r -. Float.round r) < 1e-9 then []
               else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                       ~keyword:"multipleOf" "not a multiple"])
        | None -> [])
    | _ -> None;
}

let make_compare keyword op : Compiler.keyword_compiler = {
  keyword;
  compile = fun ctx value ->
    match get_number value with
    | Some limit ->
      Some (fun instance path ->
        match get_number instance with
        | Some v ->
          if op v limit then []
          else [Error.make ~instance_path:path ~schema_path:ctx.schema_path
                  ~keyword (Printf.sprintf "expected %s %g" keyword limit)]
        | None -> [])
    | _ -> None;
}

let compile_maximum = make_compare "maximum" (fun v l -> v <= l)
let compile_exclusive_maximum = make_compare "exclusiveMaximum" (fun v l -> v < l)
let compile_minimum = make_compare "minimum" (fun v l -> v >= l)
let compile_exclusive_minimum = make_compare "exclusiveMinimum" (fun v l -> v > l)

let compilers = [
  compile_multiple_of; compile_maximum; compile_exclusive_maximum;
  compile_minimum; compile_exclusive_minimum;
]
