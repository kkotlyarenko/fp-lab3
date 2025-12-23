type algorithm = Linear | Newton

type config = {
  algorithms: algorithm list;
  step: float;
  newton_points: int;
}

type state = {
  linear_buffer: (float * float) list;
  newton_buffer: (float * float) list;
  linear_last_x: float option;
  newton_last_x: float option;
}

let initial_state = {
  linear_buffer = [];
  newton_buffer = [];
  linear_last_x = None;
  newton_last_x = None;
}

type out_chunk = string * (float * float) list

let rec drop n xs =
  if n <= 0 then xs
  else
    match xs with
    | [] -> []
    | _ :: tl -> drop (n - 1) tl

let add_to_buffer point buffer max_size =
  let new_buffer = buffer @ [point] in
  let overflow = List.length new_buffer - max_size in
  if overflow <= 0 then new_buffer else drop overflow new_buffer

let process_point config state point : state * out_chunk list =
  let (state_after_linear, out_linear) =
    if List.mem Linear config.algorithms then
      let buffer = add_to_buffer point state.linear_buffer 2 in
      if List.length buffer >= 2 then
        let results, new_last =
          Linear.linear_stream_interpolate buffer config.step state.linear_last_x
        in
        ({ state with linear_buffer = buffer; linear_last_x = new_last }, [ ("linear", results) ])
      else
        ({ state with linear_buffer = buffer }, [])
    else
      (state, [])
  in
  if List.mem Newton config.algorithms then
    let buffer = add_to_buffer point state_after_linear.newton_buffer (config.newton_points + 1) in
    if List.length buffer >= config.newton_points then
      let results, new_last =
        Newton.newton_stream_interpolate buffer config.step state_after_linear.newton_last_x config.newton_points
      in
      let buffer' = if List.length buffer > config.newton_points then List.tl buffer else buffer in
      (
        { state_after_linear with newton_buffer = buffer'; newton_last_x = new_last },
        out_linear @ [ ("newton", results) ]
      )
    else
      ({ state_after_linear with newton_buffer = buffer }, out_linear)
  else
    (state_after_linear, out_linear)

let finalize config state : out_chunk list =
  let out_linear =
    if List.mem Linear config.algorithms then
      match state.linear_buffer with
      | [_; (x2, y2)] ->
        let last_x = match state.linear_last_x with Some x -> x | None -> x2 -. config.step in
        if last_x +. config.step <= x2 +. (config.step *. 0.001) then
          [ ("linear", [ (x2, y2) ]) ]
        else
          []
      | _ -> []
    else
      []
  in
  let out_newton =
    if List.mem Newton config.algorithms then
      let results =
        Newton.newton_final_interpolate state.newton_buffer config.step state.newton_last_x
      in
      [ ("newton", results) ]
    else
      []
  in
  out_linear @ out_newton

let run config =
  let rec loop state seq =
    match seq () with
    | Seq.Nil ->
      let outs = finalize config state in
      List.iter (fun (name, pts) -> Output.print_points name pts) outs
    | Seq.Cons (point, rest) ->
      let state', outs = process_point config state point in
      List.iter (fun (name, pts) -> Output.print_points name pts) outs;
      loop state' rest
  in
  loop initial_state Input.input_point_seq
