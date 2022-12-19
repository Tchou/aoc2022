open Utils.Syntax.Hashtbl
open Utils

module Graph : sig
  include GRAPH

  val load_level : unit -> v list * v * v * t
end = struct
  type v = int * int
  type t = int array array

  let iter_vertices g f =
    for r = 0 to Array.length g - 1 do
      for c = 0 to Array.length g.(0) - 1 do
        f (r, c)
      done
    done

  let in_range g (r, c) =
    r >= 0 && r < Array.length g && c >= 0 && c < Array.length g.(0)

  let moves = [| -1, 0; 1, 0; 0, -1; 0, 1 |]
  let fold_succ _ _ _ _ = assert false

  let iter_succ g (r, c) f =
    for k = 0 to 3 do
      let i, j = moves.(k) in
      let ((rs, cs) as s) = i + r, j + c in
      if in_range g s && g.(r).(c) <= 1 + g.(rs).(cs) then f (s, 1)
    done

  let load_level () =
    let lowest_points = ref [] in
    let start = ref (0, 0) in
    let finish = ref (0, 0) in
    let _, gl =
      Utils.fold_lines
        (fun (row, accl) l ->
          ( row + 1,
            Array.init (String.length l) (fun col ->
                let v = row, col in
                let c = l.[col] in
                if c = 'S' then start := v else if c = 'E' then finish := v;
                let c = max 0 (Char.code c - Char.code 'a') in
                if c = 0 then lowest_points := v :: !lowest_points;
                c)
            :: accl ))
        (0, [])
    in
    !lowest_points, !start, !finish, gl |> List.rev |> Array.of_list
end

module Algo = GraphAlgo (Graph)

let find_all g finish targets =
  let t0 = Unix.gettimeofday () in
  let finish_map = ~%(List.map (fun t -> t, max_int) targets) in
  let () = Algo.dijkstra g finish finish_map in
  let s = Hashtbl.fold (fun _ s acc -> min acc s) finish_map max_int in
  let t1 = Unix.gettimeofday () in
  Format.printf "%d (%fms)@\n" s (1000. *. (t1 -. t0))

module Sol = struct
  let name = "12"

  let solve_part1 () =
    let _, start, finish, g = Graph.load_level () in
    find_all g finish [ start ]

  let solve_part2 () =
    let lowest_points, _, finish, g = Graph.load_level () in
    find_all g finish lowest_points
end

let () = Solution.register_mod (module Sol)