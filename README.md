# ocaml-jsonschema

JSON Schema draft-07 validator for OCaml.

Pure OCaml implementation. Compiles schemas into validator functions for repeated use.

## Packages

- **jsonschema-core** — `$ref` resolution, JSON Pointer, registry, regex engine, instance model
- **jsonschema-validation** — All 37 draft-07 validation keywords + 17 format validators

## Installation

```
opam pin add jsonschema-core .
opam pin add jsonschema-validation .
```

## Usage

```ocaml
let schema = Yojson.Safe.from_string {|{
  "type": "object",
  "properties": {
    "name": { "type": "string" },
    "age": { "type": "integer", "minimum": 0 }
  },
  "required": ["name"]
}|}

let validate = Jsonschema_validation.compile schema

let () =
  let data = Yojson.Safe.from_string {|{"name": "Alice", "age": 30}|} in
  match validate data with
  | [] -> print_endline "valid"
  | errors -> List.iter (fun e -> print_endline (Jsonschema_core.Error.to_string e)) errors
```

## Validation keywords

- **Type**: type, enum, const
- **Numeric**: multipleOf, maximum, exclusiveMaximum, minimum, exclusiveMinimum
- **String**: maxLength, minLength, pattern
- **Array**: items, additionalItems, maxItems, minItems, uniqueItems, contains
- **Object**: properties, patternProperties, additionalProperties, maxProperties, minProperties, required, dependencies, dependentRequired, propertyNames
- **Logic**: allOf, anyOf, oneOf, not
- **Conditional**: if/then/else
- **Format**: date-time, date, time, email, idn-email, hostname, idn-hostname, uri, uri-reference, iri, iri-reference, uri-template, ipv4, ipv6, json-pointer, relative-json-pointer, regex
- **Content**: contentEncoding (base64), contentMediaType (application/json)

## Format validation

Format validators cover RFC 3339 (date/time with leap second UTC conversion), RFC 3986/3987 (URI/IRI with percent-encoding and port validation), RFC 5890/5891/5892/5893 (IDNA2008 hostnames via [ocaml-idna](https://github.com/YuriyKrasilnikov/ocaml-idna)), and RFC 5321 (email).

## Test suite results

[JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite) draft-07:

- Mandatory: 922/922 (100%)
- Format: 535/535 (100%)
- Optional: 98/118 (83.1%)
- **Total: 1555/1575 (98.7%)**

## Known limitations

20 optional test failures related to ECMA-262 regex Unicode semantics. The `re` library ([ocaml/ocaml-re](https://github.com/ocaml/ocaml-re)) operates at byte level without Unicode codepoint support. Affected patterns: `\D`, `\W`, `\S` on multi-byte characters, `\uXXXX` escapes, UTF-16 surrogate pairs, and Unicode character classes in `pattern`/`patternProperties`. Tracking: [ocaml-re#24](https://github.com/ocaml/ocaml-re/issues/24), [ocaml-re#592](https://github.com/ocaml/ocaml-re/pull/592).

## License

ISC
