open Interpolation_lib

let print_usage () =
  print_endline "Usage: interpolation [OPTIONS]";
  print_endline "";
  print_endline "Options:";
  print_endline "  --linear         Use linear interpolation";
  print_endline "  --newton         Use Newton interpolation";
  print_endline "  --step <float>   Sampling step (default: 1.0)";
  print_endline "  -n <int>         Newton window size (default: 4)";
  print_endline "  --help           Show this help";
  print_endline "";
  print_endline "Input format: x<sep>y (sep: ';', ',', tab, or space)";
  print_endline "Data is read from stdin; results are written to stdout."

let parse_args () =
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse algorithms step newton_n = function
    | [] -> (List.rev algorithms, step, newton_n, false)
    | "--help" :: _ -> (algorithms, step, newton_n, true)
    | "--linear" :: rest -> parse (Interpolation.Linear :: algorithms) step newton_n rest
    | "--newton" :: rest -> parse (Interpolation.Newton :: algorithms) step newton_n rest
    | "--step" :: v :: rest ->
      (match float_of_string_opt v with
       | Some f -> parse algorithms f newton_n rest
       | None ->
         Printf.eprintf "Error: invalid --step value '%s'\n" v;
         exit 1)
    | "-n" :: v :: rest ->
      (match int_of_string_opt v with
       | Some n -> parse algorithms step n rest
       | None ->
         Printf.eprintf "Error: invalid -n value '%s'\n" v;
         exit 1)
    | arg :: rest ->
      Printf.eprintf "Warning: unknown argument '%s'\n" arg;
      parse algorithms step newton_n rest
  in
  let algorithms, step, newton_points, show_help = parse [] 1.0 4 args in
  if show_help then (
    print_usage ();
    exit 0
  );
  if step <= 0.0 then (
    Printf.eprintf "Error: --step must be > 0\n";
    exit 1
  );
  if newton_points < 2 then (
    Printf.eprintf "Error: -n must be >= 2\n";
    exit 1
  );
  let algorithms = if algorithms = [] then [Interpolation.Linear] else algorithms in
  { Interpolation.algorithms; step; newton_points }

let print_point algorithm (x, y) =
  Printf.printf "%s: %s\t%s\n%!" algorithm (Output.format_float x) (Output.format_float y)

let print_points algorithm points =
  List.iter (print_point algorithm) points

let run_streaming config =
  let read_line_opt () =
    try Some (input_line stdin) with End_of_file -> None
  in
  let rec loop state =
    match read_line_opt () with
    | None ->
      let outs = Interpolation.finalize config state in
      List.iter (fun (name, pts) -> print_points name pts) outs
    | Some line ->
      (match Input.parse_line line with
       | None -> loop state
       | Some point ->
         let state', outs = Interpolation.process_point config state point in
         List.iter (fun (name, pts) -> print_points name pts) outs;
         loop state')
  in
  loop Interpolation.initial_state

let () =
  let config = parse_args () in
  run_streaming config

