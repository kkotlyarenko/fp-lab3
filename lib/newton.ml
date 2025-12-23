type point = float * float

let divided_differences points =
  let xs = Array.of_list (List.map fst points) in
  let ys = List.map snd points in
  let n = Array.length xs in
  let next_diffs order prev =
    let prev_arr = Array.of_list prev in
    let len = Array.length prev_arr in
    let rec build i acc =
      if i >= len - 1 then List.rev acc
      else
        let denom = xs.(i + order) -. xs.(i) in
        let value = (prev_arr.(i + 1) -. prev_arr.(i)) /. denom in
        build (i + 1) (value :: acc)
    in
    build 0 []
  in
  match ys with
  | [] -> []
  | a0 :: _ ->
    let rec loop order prev coeffs_rev =
      if order >= n then List.rev coeffs_rev
      else
        let diffs = next_diffs order prev in
        match diffs with
        | [] -> List.rev coeffs_rev
        | a :: _ -> loop (order + 1) diffs (a :: coeffs_rev)
    in
    loop 1 ys [a0]

let newton_evaluate points coeffs x =
  let xs = Array.of_list (List.map fst points) in
  let coeffs_arr = Array.of_list coeffs in
  let n = Array.length coeffs_arr in
  let rec aux i acc prod =
    if i >= n then acc
    else
      let prod' = prod *. (x -. xs.(i - 1)) in
      aux (i + 1) (acc +. coeffs_arr.(i) *. prod') prod'
  in
  if n = 0 then 0.0 else aux 1 coeffs_arr.(0) 1.0

let generate_points start stop step =
  let rec aux current acc =
    if current > stop +. (step *. 0.001) then List.rev acc
    else aux (current +. step) (current :: acc)
  in
  aux start []

let newton_interpolation points step =
  match points with
  | [] | [_] -> []
  | _ ->
    let coeffs = divided_differences points in
    let xs_list = List.map fst points in
    let x_min = List.fold_left min max_float xs_list in
    let x_max = List.fold_left max min_float xs_list in
    let xs = generate_points x_min x_max step in
    List.map (fun x -> (x, newton_evaluate points coeffs x)) xs

let newton_stream_interpolate points step last_output_x window_size =
  let n = List.length points in
  if n < window_size then ([], last_output_x)
  else
    let window = 
      let rec take n lst =
        match lst with
        | [] -> []
        | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
      in
      take window_size points
    in
    let coeffs = divided_differences window in
    let xs_list = List.map fst window in
    let x_min = List.fold_left min max_float xs_list in
    let x_max = List.fold_left max min_float xs_list in
    
    let start_x = 
      match last_output_x with
      | None -> x_min
      | Some lx -> 
        let next = lx +. step in
        if next < x_min then x_min else next
    in
    
    if start_x > x_max then ([], last_output_x)
    else
      let end_x = 
        if n > window_size then
          let next_window_start = List.nth xs_list 1 in
          next_window_start
        else
          x_max
      in
      let xs = generate_points start_x end_x step in
      let xs_filtered = 
        if n > window_size then
          List.filter (fun x -> x < end_x -. (step *. 0.001)) xs
        else
          xs
      in
      let result = List.map (fun x -> (x, newton_evaluate window coeffs x)) xs_filtered in
      let new_last = 
        match List.rev result with
        | (lx, _) :: _ -> Some lx
        | [] -> last_output_x
      in
      (result, new_last)

let newton_final_interpolate points step last_output_x =
  match points with
  | [] | [_] -> []
  | _ ->
    let coeffs = divided_differences points in
    let xs_list = List.map fst points in
    let x_max = List.fold_left max min_float xs_list in
    
    let start_x = 
      match last_output_x with
      | None -> List.fold_left min max_float xs_list
      | Some lx -> lx +. step
    in
    
    if start_x > x_max then []
    else
      let xs = generate_points start_x x_max step in
      List.map (fun x -> (x, newton_evaluate points coeffs x)) xs
