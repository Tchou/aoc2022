let rec fold_lines f acc =
  match read_line () with
  | s ->
      let acc = f acc s in
      fold_lines f acc
  | exception End_of_file -> acc

let rec fold_fields sep f acc =
  match String.split_on_char sep (read_line ()) with
  | l ->
      let acc = f acc l in
      fold_fields sep f acc
  | exception End_of_file -> acc

let rec fold_chars f acc =
  match input_char stdin with
  | c -> fold_chars f (f acc c)
  | exception End_of_file -> acc

let clear_screen = "\x1b[1;1H\x1b[2J"
let compare_fst x y = Stdlib.compare (fst x) (fst y)
let compare_snd x y = Stdlib.compare (snd x) (snd y)

module Syntax = struct
  module Hashtbl = struct
    module Hashtbl = struct
      include Hashtbl

      let iter_all f h =
        iter (fun k _ -> Hashtbl.find_all h k |> List.iter (fun v -> f k v)) h
    end

    type delete = Delete

    let ( ~% ) l =
      let h = Hashtbl.create 16 in
      List.iter (fun (a, b) -> Hashtbl.add h a b) l;
      h

    let ( .%[]<- ) h k v = Hashtbl.replace h k v
    let ( .%[] ) h k = Hashtbl.find h k
    let ( .%![]<- ) h k v = Hashtbl.add h k v
    let ( .%![] ) h k = Hashtbl.find_all h k
    let ( .%?[] ) h k = Hashtbl.mem h k
    let ( .%*[]<- ) h k Delete = Hashtbl.remove h k
  end
end

module type GRAPH = sig
  type v
  type t

  val iter_vertices : t -> (v -> unit) -> unit
  val iter_succ : t -> v -> (v * int -> unit) -> unit
  val fold_succ : t -> v -> ('acc -> v * int -> 'acc) -> 'acc -> 'acc
end

module GraphAlgo (Graph : GRAPH) = struct
  open Syntax.Hashtbl

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
      t.by_vertex.%*[v] <- Delete;
      t.by_prio <- nqueue;
      res

    let create () = { by_prio = Set.empty; by_vertex = ~%[] }
    let is_empty t = Set.is_empty t.by_prio

    let add t ((prio, v) as e) =
      try
        let old_prio = t.by_vertex.%[v] in
        if old_prio > prio then begin
          t.by_prio <- Set.add e (Set.remove (old_prio, v) t.by_prio);
          t.by_vertex.%[v] <- prio
        end
      with Not_found ->
        t.by_prio <- Set.add e t.by_prio;
        t.by_vertex.%[v] <- prio
  end

  let path_length t last =
    let rec loop acc v =
      match t.%[v] with v2 -> loop (1 + acc) v2 | exception Not_found -> acc
    in
    loop 0 last

  let add_dist d1 d2 =
    let d = d1 + d2 in
    if d < 0 then max_int else d

  let floyd_warshall g =
    let dist = ~%[] in
    let () =
      Graph.iter_vertices g (fun v ->
          dist.%[v, v] <- 0;
          Graph.iter_succ g v (fun (w, n) -> dist.%[v, w] <- n))
    in
    let get_dist i j = try dist.%[i, j] with Not_found -> max_int in
    let add_dist d1 d2 =
      if d1 == max_int || d2 == max_int then max_int else d1 + d2
    in
    Graph.iter_vertices g (fun k ->
        Graph.iter_vertices g (fun i ->
            Graph.iter_vertices g (fun j ->
                let sum_sub_path = add_dist (get_dist i k) (get_dist k j) in
                if get_dist i j > sum_sub_path then dist.%[i, j] <- sum_sub_path)));
    dist

  let reverse_floyd_warshall g dist =
    let rdist = ~%[] in
    Hashtbl.iter
      (fun (i, j) d ->
        let dist_array = if d <= 1 then [||] else Array.make d None in
        rdist.%[i, j] <- dist_array;
        for d' = 1 to d - 1 do
          try
            Graph.iter_vertices g (fun k ->
                if dist.%[i, k] == d' && dist.%[k, j] == d - d' && d >=0 then begin
                  dist_array.(d') <- Some k; raise_notrace Exit
                end)
          with Exit -> ()
        done)
      dist;
    rdist

  let dijkstra g start finish_map =
    let todo = ref (Hashtbl.length finish_map) in
    let prev = ~%[] in
    let dist = ~%[] in
    let get_dist v = try dist.%[v] with Not_found -> max_int in
    let queue = Pqueue.create () in
    let () =
      Graph.iter_vertices g (fun v -> Pqueue.add queue (max_int, v));
      Pqueue.add queue (0, start);
      dist.%[start] <- 0
    in
    match
      while not (Pqueue.is_empty queue) do
        let _, u = Pqueue.remove_min queue in
        if finish_map.%?[u] && prev.%?[u] then begin
          let l = path_length prev u in
          finish_map.%[u] <- l;
          decr todo;
          if !todo = 0 then raise Exit
        end;
        Graph.iter_succ g u (fun (v, d) ->
            let v_dist = get_dist v in
            let alt = add_dist (get_dist u) d in
            if alt < v_dist then begin
              prev.%[v] <- u;
              dist.%[v] <- alt;
              Pqueue.add queue (alt, v)
            end)
      done
    with
    | () | (exception Exit) -> ()
end
