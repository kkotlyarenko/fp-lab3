open Interpolation_lib.Newton

let eps = 1e-6

let test_divided_differences () =
  let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 2.0) ] in
  match divided_differences points with
  | [a0; a1; a2] ->
    Alcotest.(check (float eps)) "a0" 0.0 a0;
    Alcotest.(check (float eps)) "a1" 1.0 a1;
    Alcotest.(check (float eps)) "a2" 0.0 a2
  | _ -> Alcotest.fail "unexpected number of coefficients"

let test_newton_evaluate () =
  let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0) ] in
  let coeffs = divided_differences points in
  let actual = newton_evaluate points coeffs 1.5 in
  Alcotest.(check (float eps)) "x^2 at 1.5" 2.25 actual

let test_newton_interpolation () =
  let points = [ (0.0, 0.0); (1.0, 1.0); (2.0, 2.0) ] in
  let result = newton_interpolation points 0.5 in
  Alcotest.(check int) "count" 5 (List.length result);
  List.iter (fun (x, y) -> Alcotest.(check (float eps)) "identity" x y) result

let () =
  Alcotest.run "newton" [
    ("newton", [
      Alcotest.test_case "divided_differences" `Quick (fun () -> test_divided_differences ());
      Alcotest.test_case "evaluate" `Quick (fun () -> test_newton_evaluate ());
      Alcotest.test_case "interpolation" `Quick (fun () -> test_newton_interpolation ());
    ])
  ]
