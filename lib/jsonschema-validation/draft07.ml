(** Draft-07 vocabulary — all keyword compilers. *)

let all_keywords : Jsonschema_core.Compiler.keyword_compiler list = [
  Kw_type.compilers;
  Kw_numeric.compilers;
  Kw_string.compilers;
  Kw_array.compilers;
  Kw_object.compilers;
  Kw_logic.compilers;
  Kw_conditional.compilers;
  Kw_format.compilers;
  Kw_content.compilers;
] |> List.flatten
