type point = float * float

let format_float f =
  let s = Printf.sprintf "%.6f" f in
  let len = String.length s in
  let rec find_end i =
    if i < 0 then 0
    else if s.[i] = '0' then find_end (i - 1)
    else if s.[i] = '.' then i
    else i + 1
  in
  String.sub s 0 (find_end (len - 1))

let print_point algorithm (x, y) =
  Printf.printf "%s: %s\t%s\n%!" algorithm (format_float x) (format_float y)

let print_points algorithm points =
  List.iter (print_point algorithm) points
