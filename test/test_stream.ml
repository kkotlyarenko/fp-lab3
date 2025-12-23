open Interpolation_lib

let eps = 1e-6

let float_pair = Alcotest.pair (Alcotest.float eps) (Alcotest.float eps)

let find_chunk name chunks =
  chunks
  |> List.filter (fun (n, _) -> String.equal n name)
  |> List.map snd
  |> List.concat

let test_linear_stream_no_duplicate_endpoint () =
  (* step=1, points (0,0) then (2,2): output should include x=0,1 but not x=2 (endpoint printed on finalize) *)
  let step = 1.0 in
  let p1 = (0.0, 0.0) in
  let p2 = (2.0, 2.0) in

  let (pts1, last1) = Linear.linear_stream_interpolate [p1] step None in
  Alcotest.(check int) "no output on single point" 0 (List.length pts1);
  Alcotest.(check (option (float eps))) "last unchanged" None last1;

  let (pts2, last2) = Linear.linear_stream_interpolate [p1; p2] step None in
  Alcotest.(check int) "two points output" 2 (List.length pts2);
  Alcotest.(check float_pair) "x0" (0.0, 0.0) (List.nth pts2 0);
  Alcotest.(check float_pair) "x1" (1.0, 1.0) (List.nth pts2 1);
  Alcotest.(check (option (float eps))) "last" (Some 1.0) last2

let test_interpolation_process_point_linear_finalize_endpoint () =
  let cfg : Interpolation.config = { algorithms = [Interpolation.Linear]; step = 1.0; newton_points = 4 } in
  let s0 = Interpolation.initial_state in
  let (s1, out1) = Interpolation.process_point cfg s0 (0.0, 0.0) in
  Alcotest.(check int) "no output" 0 (List.length (find_chunk "linear" out1));
  let (_s2, out2) = Interpolation.process_point cfg s1 (2.0, 2.0) in
  Alcotest.(check int) "stream output count" 2 (List.length (find_chunk "linear" out2));

  let outs_final = Interpolation.finalize cfg { (Interpolation.process_point cfg s1 (2.0, 2.0) |> fst) with linear_buffer = [(0.0,0.0);(2.0,2.0)] } in
  (* finalize should contain endpoint x=2,y=2 *)
  let final_linear = find_chunk "linear" outs_final in
  match final_linear with
  | [p] -> Alcotest.(check float_pair) "endpoint" (2.0, 2.0) p
  | _ -> Alcotest.fail "expected exactly one endpoint"

let test_newton_stream_requires_window () =
  let step = 0.5 in
  let window = 3 in
  let points2 = [ (0.0, 0.0); (1.0, 1.0) ] in
  let (out, last) = Newton.newton_stream_interpolate points2 step None window in
  Alcotest.(check int) "not enough points" 0 (List.length out);
  Alcotest.(check (option (float eps))) "last" None last

let test_newton_stream_outputs_on_window () =
  let step = 1.0 in
  let window = 3 in
  let points3 = [ (0.0, 0.0); (1.0, 1.0); (2.0, 4.0) ] in
  let (out, _last) = Newton.newton_stream_interpolate points3 step None window in
  (* should at least output at x=0 and x=1 (end_x is x_max because no next window) *)
  Alcotest.(check bool) "non-empty" true (List.length out >= 2)

let () =
  Alcotest.run "stream" [
    ("linear", [
      Alcotest.test_case "no duplicate endpoint" `Quick test_linear_stream_no_duplicate_endpoint;
      Alcotest.test_case "process_point + finalize" `Quick test_interpolation_process_point_linear_finalize_endpoint;
    ]);
    ("newton", [
      Alcotest.test_case "requires window" `Quick test_newton_stream_requires_window;
      Alcotest.test_case "outputs on window" `Quick test_newton_stream_outputs_on_window;
    ])
  ]
