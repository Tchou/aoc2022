open Utils.Syntax.Hashtbl

let dirs = [ 0, 1; 0, -1; -1, 0; 1, 0; 0, 0 ]

let dirchar d =
  match d with
  | 0, 1 -> '>'
  | 0, -1 -> '<'
  | 1, 0 -> 'v'
  | -1, 0 -> '^'
  | _ -> '?'

let pp_grid fmt (grid, width, height) =
  Format.fprintf fmt "%s@\n" (String.make (2 + width) '#');
  for i = 0 to height - 1 do
    Format.fprintf fmt "#";
    for j = 0 to width - 1 do
      match grid.%[i, j] with
      | [ d ] -> Format.fprintf fmt "%c" (dirchar d)
      | _ :: _ as l -> Format.fprintf fmt "%d" (min 9 (List.length l))
      | _ -> Format.fprintf fmt "?"
      | exception Not_found -> Format.fprintf fmt "."
    done;
    Format.fprintf fmt "#@\n%!"
  done;
  Format.fprintf fmt "%s@\n%!" (String.make (2 + width) '#')

let advance grid width height =
  let ngrid = ~%[] in
  if false then Format.eprintf "%a@\n--@\n%!" pp_grid (grid, width, height);
  Hashtbl.iter
    (fun (r, c) dirs ->
      List.iter
        (fun (i, j) ->
          let nc = (c + j + width) mod width in
          let nr = (r + i + height) mod height in
          ngrid.%[nr, nc] <-
            (i, j) :: (try ngrid.%[nr, nc] with Not_found -> []))
        dirs)
    grid;
  ngrid

let valid start exit grid width height (r, c) =
  (r, c) = start
  || (r, c) = exit
  || (r >= 0 && c >= 0 && r < height && c < width && not grid.%?[r, c])

let find_period grid width height =
  let mega_table = ~%[] in
  let rec loop grid n =
    let points = grid |> Hashtbl.to_seq |> List.of_seq |> List.sort compare in
    if mega_table.%?[points] then n
    else begin
      mega_table.%[points] <- points;
      let ngrid = advance grid width height in
      loop ngrid (n + 1)
    end
  in
  loop grid 0

let create_digrpah start exit width height period grid =
  let graph = ~%[] in
  let maps = ~%[] in
  let goals = ref [] in
  let rec loop grid n ((r, c) as point) =
    if not graph.%?[point, n] then begin
      if point = exit then begin
        goals := (point, n) :: !goals
      end;
      let ngrid =
        try maps.%[n mod period]
        with Not_found ->
          let ngrid = advance grid width height in
          maps.%[n] <- ngrid; ngrid
      in
      let dirs = [ 0, 1; 0, -1; -1, 0; 1, 0; 0, 0 ] in
      let todo = ref [] in
      let () =
        graph.%[point, n] <- (try graph.%[point, n] with Not_found -> [])
      in
      List.iter
        (fun (i, j) ->
          let dest = r + i, c + j in
          if valid start exit ngrid width height dest then begin
            graph.%[point, n] <- (dest, (n + 1) mod period) :: graph.%[point, n];
            todo := dest :: !todo
          end)
        dirs;
      List.iter (loop ngrid ((n + 1) mod period)) !todo
    end
  in
  loop grid 0 start; graph, !goals

let load_level () =
  let start = ref (0, 0) in
  let exit = ref (0, 0) in
  let width = ref (-1) in
  let grid = ~%[] in
  let height =
    Utils.fold_lines
      (fun row line ->
        if row < 0 then start := String.index line '.', -1;
        let hash = ref 0 in
        if !width < 0 then width := String.length line - 2;
        String.iteri
          (fun col c ->
            let col = col - 1 in
            match c with
            | '<' -> grid.%[row, col] <- [ 0, -1 ]
            | '>' -> grid.%[row, col] <- [ 0, 1 ]
            | 'v' -> grid.%[row, col] <- [ 1, 0 ]
            | '^' -> grid.%[row, col] <- [ -1, 0 ]
            | '#' -> incr hash
            | _ -> ())
          line;
        if !hash > 2 then exit := row, String.index line '.' - 1;
        row + 1)
      (-1)
  in
  !start, !exit, !width, height - 1, grid

module Graph = struct
  type t = ((int * int) * int, ((int * int) * int) list) Hashtbl.t
  type v = (int * int) * int

  let iter_vertices g f =
    Hashtbl.fold (fun v vl acc -> List.rev_append vl (v :: acc)) g []
    |> List.sort_uniq compare |> List.iter f

  let iter_succ g v f =
    List.iter
      (fun ((a, b), c) -> f (((a, b), c), 1))
      (try g.%[v] with Not_found -> [])

  let fold_succ g v f acc =
    List.fold_left
      (fun acc v -> f acc (v, 1))
      acc
      (try g.%[v] with Not_found -> [])
end

module Algo = Utils.GraphAlgo (Graph)

let pp_node fmt ((a, b), c) = Format.fprintf fmt "%d,%d %d" a b c

module Sol = struct
  let name = "24"

  let solve_part1 () =
    let start, exit, width, height, grid = load_level () in
    let () =
      Format.eprintf "START is : %d %d\n%!" (fst start) (snd start);
      Format.eprintf "EXIT is : %d %d w=%d, h=%d, @\n%!" (fst exit) (snd exit)
        width height
    in
    let period = find_period grid width height in
    let graph, goals = create_digrpah start exit width height period grid in
    let finish_map = ~%(List.map (fun p -> p, (max_int, [])) goals) in
    let () = Algo.dijkstra graph (start, 0) finish_map in
    let res = Hashtbl.fold (fun _ (n, _) acc -> min n acc) finish_map max_int in

    Format.eprintf "%d@\n%!" res

  let solve_part2 () =
    let start, exit, width, height, grid = load_level () in
    let period = find_period grid width height in
    let graph, goals = create_digrpah start exit width height period grid in
    let finish_map = ~%(List.map (fun p -> p, (max_int, [])) goals) in
    let () = Algo.dijkstra graph (start, 0) finish_map in
    let (start1, round1), len1 =
      Hashtbl.fold
        (fun p (n, _) (pacc, nacc) -> if n < nacc then p, n else pacc, nacc)
        finish_map
        ((start, 0), max_int)
    in
    Format.eprintf "LEN1: %d@\n%!" len1;
    let finish_map1 = ~%[] in
    let () =
      for i = 0 to period - 1 do
        if graph.%?[start, i] then finish_map1.%[start, i] <- max_int, []
      done
    in
    let () = Algo.dijkstra graph (start1, round1) finish_map1 in
    let (start2, round2), len2 =
      Hashtbl.fold
        (fun p (n, _) (pacc, nacc) -> if n < nacc then p, n else pacc, nacc)
        finish_map1
        ((start, 0), max_int)
    in
    Format.eprintf "LEN2: %d@\n%!" len2;
    let finish_map2 = ~%[] in
    let () =
      for i = 0 to period - 1 do
        if graph.%?[exit, i] then finish_map2.%[exit, i] <- max_int, []
      done
    in
    let () = Algo.dijkstra graph (start2, round2) finish_map2 in
    let _, len3 =
      Hashtbl.fold
        (fun p (n, _) (pacc, nacc) -> if n < nacc then p, n else pacc, nacc)
        finish_map2
        ((start, 0), max_int)
    in
    Format.eprintf "LEN3: %d@\n%!" len3;
    Format.printf "%d@\n%!" (len1 + len2 + len3)
end

let () = Solution.register_mod (module Sol)