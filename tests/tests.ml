open Spdx_licenses

let fmt = Printf.sprintf

let pp = function
  | Ok x -> fmt "Ok (%s)" (to_string x)
  | Error `ParseError -> "Error `ParseError"
  | Error (`InvalidLicenseID id) -> fmt "Error (`InvalidLicenseID %s)" id
  | Error (`InvalidExceptionID id) -> fmt "Error (`InvalidExceptionID %s)" id

let test_bool name v =
  Alcotest.test_case name `Quick (fun () -> Alcotest.(check bool) name v true)

let test name v x =
  Alcotest.test_case name `Quick
    (fun () -> Alcotest.(check string) name (pp v) x)

let () =
  Alcotest.run "Tests" [
    "tests", [
      test_bool "valid_license_ids is reasonable"
        (List.mem "MIT" valid_license_ids);
      test_bool "valid_exception_ids is reasonable"
        (List.mem "OCaml-LGPL-linking-exception" valid_exception_ids);
      test "parse fails on invalid licenses"
        (parse "TEST")
        "Error (`InvalidLicenseID TEST)";
      test "parse fails on invalid exceptions"
        (parse "MIT WITH TEST")
        "Error (`InvalidExceptionID TEST)";
      test "parse has the right precedence rule"
        (parse "LGPL-2.1-only OR BSD-3-Clause AND MIT")
        "Ok (LGPL-2.1-only OR (BSD-3-Clause AND MIT))";
    ]
  ]
