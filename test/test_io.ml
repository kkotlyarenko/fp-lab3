open Interpolation_lib

let eps = 1e-6

let test_parse_line_csv_semicolon () =
  match Input.parse_line "1.5;2.5" with
  | Some (x, y) ->
    Alcotest.(check (float eps)) "x" 1.5 x;
    Alcotest.(check (float eps)) "y" 2.5 y
  | None -> Alcotest.fail "expected Some point"

let test_parse_line_csv_comma () =
  match Input.parse_line "1.5,2.5" with
  | Some (x, y) ->
    Alcotest.(check (float eps)) "x" 1.5 x;
    Alcotest.(check (float eps)) "y" 2.5 y
  | None -> Alcotest.fail "expected Some point"

let test_parse_line_tab () =
  match Input.parse_line "1\t2" with
  | Some (x, y) ->
    Alcotest.(check (float eps)) "x" 1.0 x;
    Alcotest.(check (float eps)) "y" 2.0 y
  | None -> Alcotest.fail "expected Some point"

let test_parse_line_space () =
  match Input.parse_line "  1   2  " with
  | Some (x, y) ->
    Alcotest.(check (float eps)) "x" 1.0 x;
    Alcotest.(check (float eps)) "y" 2.0 y
  | None -> Alcotest.fail "expected Some point"

let test_parse_line_header_is_ignored () =
  Alcotest.(check (option (pair (float eps) (float eps)))) "header" None (Input.parse_line "x;y")

let test_parse_line_empty_is_ignored () =
  Alcotest.(check (option (pair (float eps) (float eps)))) "empty" None (Input.parse_line "   ")

let test_format_float_trims_zeros () =
  Alcotest.(check string) "1.500000 -> 1.5" "1.5" (Output.format_float 1.5)

let test_format_float_trims_dot () =
  Alcotest.(check string) "2.000000 -> 2" "2" (Output.format_float 2.0)

let test_format_float_rounding () =
  (* six decimals *)
  Alcotest.(check string) "round" "0.333333" (Output.format_float (1.0 /. 3.0))

let () =
  Alcotest.run "io" [
    ("input", [
      Alcotest.test_case "parse ;" `Quick test_parse_line_csv_semicolon;
      Alcotest.test_case "parse ," `Quick test_parse_line_csv_comma;
      Alcotest.test_case "parse tab" `Quick test_parse_line_tab;
      Alcotest.test_case "parse space" `Quick test_parse_line_space;
      Alcotest.test_case "ignore header" `Quick test_parse_line_header_is_ignored;
      Alcotest.test_case "ignore empty" `Quick test_parse_line_empty_is_ignored;
    ]);
    ("output", [
      Alcotest.test_case "format trims zeros" `Quick test_format_float_trims_zeros;
      Alcotest.test_case "format trims dot" `Quick test_format_float_trims_dot;
      Alcotest.test_case "format rounding" `Quick test_format_float_rounding;
    ])
  ]
