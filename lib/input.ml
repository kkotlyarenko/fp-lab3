type point = float * float

let parse_line line =
  let line = String.trim line in
  if String.length line = 0 then None
  else
    let separators = [';'; ','; '\t'; ' '] in
    let try_parse sep =
      let parts = String.split_on_char sep line in
      let parts = List.filter (fun s -> String.length (String.trim s) > 0) parts in
      match parts with
      | [x_str; y_str] ->
        (try
          let x = float_of_string (String.trim x_str) in
          let y = float_of_string (String.trim y_str) in
          Some (x, y)
        with Failure _ -> None)
      | _ -> None
    in
    let rec try_separators = function
      | [] -> None
      | sep :: rest ->
        match try_parse sep with
        | Some p -> Some p
        | None -> try_separators rest
    in
    try_separators separators

let read_line_opt () =
  try Some (input_line stdin)
  with End_of_file -> None

let rec input_point_seq () =
  match read_line_opt () with
  | None -> Seq.Nil
  | Some line ->
    match parse_line line with
    | Some point -> Seq.Cons (point, input_point_seq)
    | None -> input_point_seq ()
