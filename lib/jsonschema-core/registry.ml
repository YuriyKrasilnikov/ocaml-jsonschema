(** Schema registry — stores and resolves schemas by URI. *)

type validator = Yojson.Safe.t -> Json_pointer.t -> Error.t list
type retriever = Uri.t -> Yojson.Safe.t option

type compile_state =
  | Compiling of (validator -> unit) list ref
  | Compiled of validator

type t = {
  raw : (string, Yojson.Safe.t) Hashtbl.t;
  state : (string, compile_state) Hashtbl.t;
  retriever : retriever option;
}

let uri_key uri = Uri.to_string (Uri_util.without_fragment uri)

let create ?retriever () =
  { raw = Hashtbl.create 64;
    state = Hashtbl.create 64;
    retriever }

(** Keywords whose values are schemas or contain schemas.
    Only these are traversed when indexing $id. *)
let schema_keywords = [
  "additionalItems"; "additionalProperties"; "contains";
  "if"; "then"; "else"; "not"; "propertyNames";
]
let schema_map_keywords = [
  "properties"; "patternProperties"; "definitions";
]
let schema_array_keywords = [
  "allOf"; "anyOf"; "oneOf";
]
let schema_or_array_keywords = [
  "items";
]
let schema_or_dep_keywords = [
  "dependencies";
]

(** Recursively scan a schema for $id keywords and register sub-schemas.
    Only traverses keyword values that are schemas per draft-07. *)
let rec index_ids t ~base_uri (json : Yojson.Safe.t) =
  match json with
  | `Assoc pairs ->
    let new_base =
      match List.assoc_opt "$id" pairs with
      | Some (`String id_str) ->
        let id_uri = Uri.of_string id_str in
        let resolved = Uri_util.resolve ~base:base_uri id_uri in
        let key = Uri.to_string resolved in
        Hashtbl.replace t.raw key json;
        if Uri_util.fragment id_uri <> None && Uri.path id_uri = "" then
          base_uri
        else
          Uri_util.without_fragment resolved
      | _ -> base_uri
    in
    List.iter (fun (k, v) ->
      if List.mem k schema_keywords then
        index_ids t ~base_uri:new_base v
      else if List.mem k schema_map_keywords then
        (match v with
         | `Assoc props -> List.iter (fun (_, sv) -> index_ids t ~base_uri:new_base sv) props
         | _ -> ())
      else if List.mem k schema_array_keywords then
        (match v with
         | `List items -> List.iter (fun sv -> index_ids t ~base_uri:new_base sv) items
         | _ -> ())
      else if List.mem k schema_or_array_keywords then
        (match v with
         | `List items -> List.iter (fun sv -> index_ids t ~base_uri:new_base sv) items
         | _ -> index_ids t ~base_uri:new_base v)
      else if List.mem k schema_or_dep_keywords then
        (match v with
         | `Assoc deps ->
           List.iter (fun (_, dv) ->
             match dv with
             | `Assoc _ | `Bool _ -> index_ids t ~base_uri:new_base dv
             | _ -> ()
           ) deps
         | _ -> ())
      else if k = "$ref" then ()
      else ()
    ) pairs
  | _ -> ()

let add t uri json =
  let base_key = uri_key uri in
  Hashtbl.replace t.raw base_key json;
  index_ids t ~base_uri:(Uri_util.without_fragment uri) json

let resolve_raw t uri =
  let frag = Uri_util.fragment uri in
  let base = Uri_util.without_fragment uri in
  let base_key = Uri.to_string base in
  (* Try exact URI (with fragment for named anchors) *)
  let full_key = Uri.to_string uri in
  match Hashtbl.find_opt t.raw full_key with
  | Some _ as found -> found
  | None ->
    (* Try base document + JSON Pointer fragment *)
    let doc =
      match Hashtbl.find_opt t.raw base_key with
      | Some _ as found -> found
      | None ->
        (* Call retriever with base URI (without fragment). *)
        match t.retriever with
        | Some f ->
          let result = f base in
          (match result with
           | Some json ->
             Hashtbl.replace t.raw base_key json;
             (* Index the fetched document so its $id/definitions are known *)
             index_ids t ~base_uri:base json;
             Some json
           | None -> None)
        | None -> None
    in
    match doc, frag with
    | Some doc, Some frag when frag <> "" && frag.[0] = '/' ->
      (* JSON Pointer fragment *)
      (match Json_pointer.of_string frag with
       | Ok ptr -> Json_pointer.resolve ptr doc
       | Error _ -> None)
    (* Empty fragment or no fragment = whole document. *)
    | Some _ as found, (None | Some "") -> found
    | Some _, Some _ -> None  (* non-pointer fragment, not found as named anchor *)
    | None, _ -> None

(** Walk a JSON Pointer path, tracking $id changes along the way. *)
let resolve_pointer_with_base ~base_uri ptr doc =
  let rec walk base_uri json = function
    | [] -> Some (json, base_uri)
    | token :: rest ->
      (* Check if current node has $id that changes base *)
      let base_uri = match json with
        | `Assoc pairs ->
          (match List.assoc_opt "$id" pairs with
           | Some (`String id_str) ->
             let id_uri = Uri.of_string id_str in
             if Uri_util.fragment id_uri <> None && Uri.path id_uri = "" then
               base_uri
             else
               Uri_util.without_fragment (Uri_util.resolve ~base:base_uri id_uri)
           | _ -> base_uri)
        | _ -> base_uri
      in
      match json with
      | `Assoc pairs ->
        (match List.assoc_opt token pairs with
         | Some child -> walk base_uri child rest
         | None -> None)
      | `List items ->
        (match int_of_string_opt token with
         | Some i when i >= 0 && i < List.length items ->
           walk base_uri (List.nth items i) rest
         | _ -> None)
      | _ -> None
  in
  walk base_uri doc ptr

let resolve_with_base t uri =
  let frag = Uri_util.fragment uri in
  let base = Uri_util.without_fragment uri in
  let base_key = Uri.to_string base in
  (* Try exact URI first (named anchors) *)
  let full_key = Uri.to_string uri in
  match Hashtbl.find_opt t.raw full_key with
  | Some json -> Some (json, base)
  | None ->
    let doc =
      match Hashtbl.find_opt t.raw base_key with
      | Some _ as found -> found
      | None ->
        match t.retriever with
        | Some f ->
          (match f base with
           | Some json ->
             Hashtbl.replace t.raw base_key json;
             index_ids t ~base_uri:base json;
             Some json
           | None -> None)
        | None -> None
    in
    match doc, frag with
    | Some doc, Some frag when frag <> "" && frag.[0] = '/' ->
      (match Json_pointer.of_string frag with
       | Ok ptr -> resolve_pointer_with_base ~base_uri:base ptr doc
       | Error _ -> None)
    | Some _ as found, (None | Some "") ->
      (match found with Some j -> Some (j, base) | None -> None)
    | _ -> None

let set_compiled t uri v =
  let key = Uri.to_string uri in
  let callbacks =
    match Hashtbl.find_opt t.state key with
    | Some (Compiling cbs) -> !cbs
    | _ -> []
  in
  Hashtbl.replace t.state key (Compiled v);
  List.iter (fun cb -> cb v) callbacks

let get_compiled t uri =
  let key = Uri.to_string uri in
  match Hashtbl.find_opt t.state key with
  | Some (Compiled v) -> Some v
  | _ -> None

let is_compiling t uri =
  let key = Uri.to_string uri in
  match Hashtbl.find_opt t.state key with
  | Some (Compiling _) -> true
  | _ -> false

let mark_compiling t uri =
  let key = Uri.to_string uri in
  Hashtbl.replace t.state key (Compiling (ref []))

let on_compiled t uri cb =
  let key = Uri.to_string uri in
  match Hashtbl.find_opt t.state key with
  | Some (Compiling cbs) -> cbs := cb :: !cbs
  | _ -> ()
