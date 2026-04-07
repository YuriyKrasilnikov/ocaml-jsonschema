(lang dune 3.0)

(name jsonschema)

(generate_opam_files true)

(license ISC)

(authors "Yuriy Krasilnikov")

(maintainers "Yuriy Krasilnikov")

(source (github YuriyKrasilnikov/ocaml-jsonschema))

(homepage "https://github.com/YuriyKrasilnikov/ocaml-jsonschema")

(bug_reports "https://github.com/YuriyKrasilnikov/ocaml-jsonschema/issues")

(package
 (name jsonschema-core)
 (synopsis "JSON Schema core: $ref resolution, JSON Pointer, instance model")
 (depends
  (ocaml (>= 5.1))
  (yojson (>= 2.0))
  (uri (>= 4.0))
  (re (>= 1.10))
  (alcotest :with-test)))

(package
 (name jsonschema-validation)
 (synopsis "JSON Schema draft-07 validation keywords")
 (depends
  (ocaml (>= 5.1))
  (jsonschema-core (= :version))
  (re (>= 1.10))
  (ipaddr (>= 5.0))
  (domain-name (>= 0.4))
  (ptime (>= 1.0))
  (idna (>= 0.2))
  (alcotest :with-test)))
