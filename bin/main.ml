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
      (match Parsers.parse_line line with
       | None -> loop state
       | Some point ->
         let state', outs = Interpolation.process_point config state point in
         List.iter (fun (name, pts) -> print_points name pts) outs;
         loop state')
  in
  loop Interpolation.initial_state

let config_of_args (args : Parsers.args) : Interpolation.config =
  let algorithms =
    (if args.linear then [Interpolation.Linear] else []) @
    (if args.newton then [Interpolation.Newton] else [])
  in
  let algorithms = if algorithms = [] then [Interpolation.Linear] else algorithms in
  { Interpolation.algorithms; step = args.step; newton_points = args.newton_points }

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  match Parsers.parse_args argv with
  | Parsers.Help ->
    print_usage ();
    exit 0
  | Parsers.Error msg ->
    Printf.eprintf "Error: %s\n" msg;
    exit 1
  | Parsers.Args args ->
    run_streaming (config_of_args args)

