open Interpolation_lib.Linear

let eps = 1e-6

let test_linear_interpolate () =
  let p1 = (0.0, 0.0) in
  let p2 = (1.0, 1.0) in
  let actual = linear_interpolate p1 p2 0.5 in
  Alcotest.(check (float eps)) "midpoint" 0.5 actual;

  let p1 = (0.0, 0.0) in
  let p2 = (2.0, 4.0) in
  let actual = linear_interpolate p1 p2 1.0 in
  Alcotest.(check (float eps)) "slope" 2.0 actual

let test_generate_points () =
  let xs = generate_points 0.0 2.0 0.5 in
  Alcotest.(check int) "count" 5 (List.length xs);
  match xs with
  | first :: _ -> Alcotest.(check (float eps)) "first" 0.0 first
  | [] -> Alcotest.fail "expected non-empty list"

let test_linear_interpolation () =
  let points = [ (0.0, 0.0); (2.0, 2.0) ] in
  let result = linear_interpolation points 0.5 in
  Alcotest.(check int) "count" 5 (List.length result)

let () =
  Alcotest.run "linear" [
    ("linear", [
      Alcotest.test_case "linear_interpolate" `Quick (fun () -> test_linear_interpolate ());
      Alcotest.test_case "generate_points" `Quick (fun () -> test_generate_points ());
      Alcotest.test_case "linear_interpolation" `Quick (fun () -> test_linear_interpolation ());
    ])
  ]
