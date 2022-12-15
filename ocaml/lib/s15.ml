let load_level () =
  Utils.fold_lines
    (fun acc line ->
      Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
        (fun xs ys xb yb -> ((xs, ys), (xb, yb)) :: acc))
    []

let mdist (a, b) (c, d) = abs (a - c) + abs (b - d)

(**
The "disk" around a sensor at point X0,Y0 with closest beacon at
distance D0 has equation :
abs (X0 - x) + abs (Y0 - y) < D0 (< since no ties)

We look for points on the line: (x, C). Substitute:
abs (X0 - x ) + abs (Y0 - C) < D0
abs (X0 - x) + < D0 - abs (Y0 - C)
if D0 - abs (Y0 - C) negative, skip (the point is too far)
  otherwise: let M = D0 - abs (Y0 - C)
 
    -M  < X0 - x < M
    -M - X0 < - x  < M - X0
    - (M - X0)  <   x < - (-M - X0)
    X0 - M < x < X0 + M
    2 M-1 integers.
But there could be overlaps, so intersect the intervals.
  1   4
    2 3             4 - 1 = 3
*)
let solve_for_y min_x max_x yC sensors =
  let ints =
    List.fold_left
      (fun acc ((x0, y0), (xb, yb)) ->
        let d0 = mdist (x0, y0) (xb, yb) in
        let m = d0 - abs (y0 - yC) in
        if m <= 0 then acc
        else
          let x_inf = max (x0 - m) min_x in
          let x_sup = min (x0 + m) max_x in
          if x_inf = x_sup then acc else (x_inf, x_sup) :: acc)
      [] sensors
  in
  let ints = List.sort compare ints in
  let rec loop = function
    | ([] | [ _ ]) as l -> l
    | (a, b) :: ((c, d) :: ll as l) ->
        if b <= c then (a, b) :: loop l else loop ((a, max b d) :: ll)
  in
  loop ints

let count_intervals = List.fold_left (fun acc (a, b) -> acc + (b - a)) 0

let find_interval _v_min _v_max l =
  let rec loop l =
    match l with
    | [] | [ _ ] -> raise Not_found
    | (_, b) :: ((c, _) :: _ as ll) -> if b < c then b + 1 else loop ll
  in
  loop l

module Sol = struct
  let name = "15"

  let solve_part1 () =
    load_level ()
    |> solve_for_y min_int max_int 10
    |> count_intervals |> Format.printf "%d@\n"

  let solve_part2 () =
    (* Brute force*)
    let sensors = load_level () in
    let v_min = 0 in
    let v_max = 4000000 in
    let point = ref (-1, -1) in
    try
      for y = v_min to v_max do
        let ints = solve_for_y v_min v_max y sensors in
        let num = count_intervals ints in
        if num <> v_max - v_min then begin
          point := find_interval v_min v_max ints, y;
          raise_notrace Exit
        end
      done;
      Format.printf "Not_found\n%!"
    with Exit ->
      let x, y = !point in
      let res = (4000000 * x) + y in
      Format.printf "%d@\n%!" res
end

let () = Solution.register_mod (module Sol)
