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

