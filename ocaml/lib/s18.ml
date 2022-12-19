open Utils.Syntax.Hashtbl

type status = Visited | Trapped | Reachable

let load_cubes () =
  Utils.fold_fields ','
    (fun table -> function
      | [ sx; sy; sz ] ->
          table.%[int_of_string sx, int_of_string sy, int_of_string sz] <- false;
          table
      | _ -> table)
    ~%[]

let directions = [| -1, 0, 0; 1, 0, 0; 0, -1, 0; 0, 1, 0; 0, 0, -1; 0, 0, 1 |]
let add3 (a, b, c) (x, y, z) = a + x, b + y, c + z

let count_faces trapped table =
  Hashtbl.fold
    (fun cube _ acc ->
      acc + 6
      - Array.fold_left
          (fun acc dir ->
            let next = add3 dir cube in
            acc + if table.%?[next] || trapped.%?[next] then 1 else 0)
          0 directions)
    table 0

let get_limits table =
  Hashtbl.fold
    (fun (x, y, z) _ (xmin, xmax, ymin, ymax, zmin, zmax) ->
      min x xmin, max x xmax, min y ymin, max y ymax, min z zmin, max z zmax)
    table
    (max_int, min_int, max_int, min_int, max_int, min_int)

let outside limits point =
  let xmin, xmax, ymin, ymax, zmin, zmax = limits in
  let x, y, z = point in
  x < xmin || x > xmax || y < ymin || y > ymax || z < zmin || z > zmax

let dfs cache cubes limits start =
  let rec loop point =
    match cache.%[point] with
    | status -> status
    | exception Not_found ->
        if outside limits point then (
          cache.%[point] <- Reachable;
          Reachable)
        else begin
          cache.%[point] <- Visited;
          let status = loop_succ point 0 in
          cache.%[point] <- status; status
        end
  and loop_succ point i =
    if i >= Array.length directions then Trapped
    else
      let dir = directions.(i) in
      let point_succ = add3 dir point in
      if cubes.%?[point_succ] then loop_succ point (i + 1)
      else
        let status = loop point_succ in
        if status = Reachable then Reachable else loop_succ point (i + 1)
  in
  loop start

module Sol = struct
  let name = "18"

  let solve_part1 () =
    let cubes = load_cubes () in
    let n = count_faces ~%[] cubes in
    Format.printf "%d@\n" n

  let solve_part2 () =
    let cubes = load_cubes () in
    let ((xmin, xmax, ymin, ymax, zmin, zmax) as limits) = get_limits cubes in
    let cache = ~%[] in
    let trapped = ~%[] in
    for x = xmin to xmax do
      for y = ymin to ymax do
        for z = zmin to zmax do
          let cube = x, y, z in
          if not cubes.%?[cube] then
            match dfs cache cubes limits cube with
            | Reachable -> ()
            | Trapped -> trapped.%[cube] <- ()
            | _ -> assert false
        done
      done
    done;
    let n = count_faces trapped cubes in
    Format.printf "%d@\n" n
end

let () = Solution.register_mod (module Sol)
