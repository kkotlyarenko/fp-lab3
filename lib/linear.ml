type point = float * float

let linear_interpolate (x1, y1) (x2, y2) x =
  if x2 -. x1 = 0.0 then y1
  else y1 +. (y2 -. y1) *. (x -. x1) /. (x2 -. x1)

let generate_points start stop step =
  let rec aux current acc =
    if current > stop +. (step *. 0.001) then List.rev acc
    else aux (current +. step) (current :: acc)
  in
  aux start []

let linear_interpolation points step =
  match points with
  | [] | [_] -> []
  | (x1, y1) :: (x2, y2) :: _ ->
    let xs = generate_points x1 x2 step in
    List.map (fun x -> (x, linear_interpolate (x1, y1) (x2, y2) x)) xs

let linear_stream_interpolate points step last_output_x =
  match points with
  | [] | [_] -> ([], last_output_x)
  | (x1, _) :: (x2, _) :: _ ->
    let start_x = 
      match last_output_x with
      | None -> x1
      | Some lx -> 
        let next = lx +. step in
        if next < x1 then x1 else next
    in
    if start_x > x2 then ([], last_output_x)
    else
      let result = 
        match points with
        | (p1 :: p2 :: _) ->
          let xs = generate_points start_x x2 step in
          let xs_filtered = List.filter (fun x -> x < x2 -. (step *. 0.001)) xs in
          List.map (fun x -> (x, linear_interpolate p1 p2 x)) xs_filtered
        | _ -> []
      in
      let new_last = 
        match List.rev result with
        | (lx, _) :: _ -> Some lx
        | [] -> last_output_x
      in
      (result, new_last)
