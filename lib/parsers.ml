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

type args = {
  linear: bool;
  newton: bool;
  step: float;
  newton_points: int;
}

type parse_result =
  | Help
  | Error of string
  | Args of args

let parse_args (argv : string list) =
  let rec parse linear newton step newton_n = function
    | [] -> Ok (linear, newton, step, newton_n)
    | "--help" :: _ -> Error `Help
    | "--linear" :: rest -> parse true newton step newton_n rest
    | "--newton" :: rest -> parse linear true step newton_n rest
    | "--step" :: v :: rest ->
      (match float_of_string_opt v with
       | Some f -> parse linear newton f newton_n rest
       | None -> Error (`Msg (Printf.sprintf "invalid --step value '%s'" v)))
    | "-n" :: v :: rest ->
      (match int_of_string_opt v with
       | Some n -> parse linear newton step n rest
       | None -> Error (`Msg (Printf.sprintf "invalid -n value '%s'" v)))
    | _ :: rest -> parse linear newton step newton_n rest
  in
  match parse false false 1.0 4 argv with
  | Error `Help -> Help
  | Error (`Msg s) -> Error s
  | Ok (linear, newton, step, newton_points) ->
    if step <= 0.0 then Error "--step must be > 0"
    else if newton_points < 2 then Error "-n must be >= 2"
    else Args { linear; newton; step; newton_points }
