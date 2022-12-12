module Graph : sig
  type v
  type t

  val iter_vertices : t -> (v -> unit) -> unit
  val iter_pred : t -> v -> (v -> unit) -> unit
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

  let iter_pred g (r, c) f =
    for k = 0 to 3 do
      let i, j = moves.(k) in
      let ((rs, cs) as s) = i + r, j + c in
      if in_range g s && g.(r).(c) <= 1 + g.(rs).(cs) then f s
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

module Pqueue = struct
  module Set = struct
    include Set.Make (struct
      type t = int * Graph.v

      let compare = compare
    end)

    let remove_min s =
      let m = min_elt s in
      m, remove m s
  end

  type t = { mutable by_prio : Set.t; by_vertex : (Graph.v, int) Hashtbl.t }

  let remove_min t =
    let ((_, v) as res), nqueue = Set.remove_min t.by_prio in
    Hashtbl.remove t.by_vertex v;
    t.by_prio <- nqueue;
    res

  let create () = { by_prio = Set.empty; by_vertex = Hashtbl.create 16 }
  let is_empty t = Set.is_empty t.by_prio

  let add t ((prio, v) as e) =
    try
      let old_prio = Hashtbl.find t.by_vertex v in
      if old_prio > prio then begin
        t.by_prio <- Set.add e (Set.remove (old_prio, v) t.by_prio);
        Hashtbl.replace t.by_vertex v prio
      end
    with Not_found ->
      t.by_prio <- Set.add e t.by_prio;
      Hashtbl.replace t.by_vertex v prio
end

let path_length t last =
  let rec loop acc v =
    match Hashtbl.find t v with
    | v2 -> loop (1 + acc) v2
    | exception Not_found -> acc
  in
  loop 0 last

let add_dist d1 d2 =
  let d = d1 + d2 in
  if d < 0 then max_int else d

let dijkstra g start finish_map =
  let todo = ref (Hashtbl.length finish_map) in
  let prev = Hashtbl.create 16 in
  let dist = Hashtbl.create 16 in
  let get_dist v = try Hashtbl.find dist v with Not_found -> max_int in
  let queue = Pqueue.create () in
  let () =
    Graph.iter_vertices g (fun v -> Pqueue.add queue (max_int, v));
    Pqueue.add queue (0, start);
    Hashtbl.replace dist start 0
  in
  match
    while not (Pqueue.is_empty queue) do
      let _, u = Pqueue.remove_min queue in
      if Hashtbl.mem finish_map u && Hashtbl.mem prev u then begin
        let l = path_length prev u in
        Hashtbl.replace finish_map u l;
        decr todo;
        if !todo = 0 then raise Exit
      end;
      Graph.iter_pred g u (fun v ->
          let v_dist = get_dist v in
          let alt = add_dist (get_dist u) 1 in
          if alt < v_dist then begin
            Hashtbl.replace prev v u;
            Hashtbl.replace dist v alt;
            Pqueue.add queue (alt, v)
          end)
    done
  with
  | () | (exception Exit) -> finish_map

let find_all g finish targets =
  let t0 = Unix.gettimeofday () in
  let finish_map = Hashtbl.create 16 in
  List.iter (fun t -> Hashtbl.replace finish_map t max_int) targets;
  let _ = dijkstra g finish finish_map in
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