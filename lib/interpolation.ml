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

let add_to_buffer point buffer max_size =
  let new_buffer = buffer @ [point] in
  if List.length new_buffer <= max_size then new_buffer
  else new_buffer |> List.rev |> List.take max_size |> List.rev

let keep_last n xs =
  xs |> List.rev |> List.take n |> List.rev

let step_linear config state point : state * out_chunk list =
  let buffer = add_to_buffer point state.linear_buffer 2 in
  if List.length buffer >= 2 then
    let results, new_last =
      Linear.linear_stream_interpolate buffer config.step state.linear_last_x
    in
    ({ state with linear_buffer = buffer; linear_last_x = new_last }, [ ("linear", results) ])
  else
    ({ state with linear_buffer = buffer }, [])

let step_newton config state point : state * out_chunk list =
  let buffer = add_to_buffer point state.newton_buffer (config.newton_points + 1) in
  if List.length buffer >= config.newton_points then
    let results, new_last =
      Newton.newton_stream_interpolate buffer config.step state.newton_last_x config.newton_points
    in
    let buffer' =
      if List.length buffer > config.newton_points then keep_last config.newton_points buffer
      else buffer
    in
    ({ state with newton_buffer = buffer'; newton_last_x = new_last }, [ ("newton", results) ])
  else
    ({ state with newton_buffer = buffer }, [])

let process_point config state point : state * out_chunk list =
  let step_for = function
    | Linear -> step_linear config
    | Newton -> step_newton config
  in
  let apply (st, outs) alg =
    let st', outs' = step_for alg st point in
    (st', outs @ outs')
  in
  List.fold_left apply (state, []) config.algorithms

let finalize config state : out_chunk list =
  let linear_out () =
    match state.linear_buffer with
    | [_; (x2, y2)] ->
      let last_x = match state.linear_last_x with Some x -> x | None -> x2 -. config.step in
      if last_x +. config.step <= x2 +. (config.step *. 0.001) then
        [ ("linear", [ (x2, y2) ]) ]
      else
        []
    | _ -> []
  in
  let newton_out () =
    let results =
      Newton.newton_final_interpolate state.newton_buffer config.step state.newton_last_x
    in
    [ ("newton", results) ]
  in
  let append acc alg =
    match alg with
    | Linear -> acc @ linear_out ()
    | Newton -> acc @ newton_out ()
  in
  List.fold_left append [] config.algorithms

