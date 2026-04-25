open Jsonschema_validation

let is_valid_format format_name value =
  let schema = `Assoc [("format", `String format_name)] in
  compile ~format_assertion:true schema |> fun validator ->
  is_valid validator (`String value)

let test_hostname_ascii_valid () =
  Alcotest.(check bool) "example.com" true
    (is_valid_format "hostname" "example.com")

let test_hostname_unicode_rejected () =
  Alcotest.(check bool) "unicode hostname rejected" false
    (is_valid_format "hostname" "mañana.com")

let test_idn_hostname_unicode_valid () =
  Alcotest.(check bool) "unicode idn hostname" true
    (is_valid_format "idn-hostname" "mañana.com")

let test_idn_hostname_fullwidth_dot_valid () =
  Alcotest.(check bool) "fullwidth dot normalized" true
    (is_valid_format "idn-hostname" "mañana。com")

let () =
  Alcotest.run "validation-format" [
    "hostname", [
      Alcotest.test_case "ascii valid" `Quick test_hostname_ascii_valid;
      Alcotest.test_case "unicode rejected" `Quick test_hostname_unicode_rejected;
    ];
    "idn-hostname", [
      Alcotest.test_case "unicode valid" `Quick test_idn_hostname_unicode_valid;
      Alcotest.test_case "fullwidth dot valid" `Quick test_idn_hostname_fullwidth_dot_valid;
    ];
  ]
