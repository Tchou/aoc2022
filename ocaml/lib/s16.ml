open Utils.Syntax.Hashtbl

module BitSet = struct
  type t = int

  let empty = 0
  let mem i s = (1 lsl i) land s != 0
  let not_mem i s = (1 lsl i) land s == 0
  let add i s = (1 lsl i) lor s

  let elements s =
    let res = ref [] in
    for i = 60 downto 0 do
      if mem i s then res := i :: !res
    done;
    !res
end

module Graph = struct
  type t = {
    edges : (int * int) list array; (* src -> dst*)
    id_to_name : string array;
    name_to_id : (string, int) Hashtbl.t;
    valves : (int, int) Hashtbl.t;
    valve_mask : int;
  }

  type v = int

  let iter_vertices g f =
    for i = 0 to Array.length g.id_to_name - 1 do
      f i
    done

  let prepare g =
    for i = 0 to Array.length g.edges - 1 do
      g.edges.(i) <-
        List.sort_uniq
          (fun (v1, c1) (v2, c2) ->
            let c = compare c2 c1 in
            if c = 0 then compare v1 v2 else c)
          g.edges.(i)
    done

  let add_edge g s t c =
    g.edges.(s) <- List.sort_uniq compare ((t, c) :: g.edges.(s))

  let compare_names valves n1 n2 =
    if n1 = n2 then 0
    else if n1 = "AA" then -1
    else if n2 = "AA" then 1
    else
      match List.mem_assoc n1 valves, List.mem_assoc n2 valves with
      | false, false | true, true -> compare n1 n2
      | true, false -> -1
      | false, true -> 1

  let build e_list val_list =
    let count = ref ~-1 in
    let name_to_id = ~%[] in
    let new_name v =
      if not name_to_id.%?[v] then begin
        incr count; name_to_id.%[v] <- !count
      end
    in
    let todo = ref [] in
    List.iter (fun (s, (t, _)) -> todo := s :: t :: !todo) e_list;
    let todo = List.sort_uniq (fun a b -> compare_names val_list a b) !todo in
    List.iter new_name todo;
    let id_to_name = Array.make (!count + 1) "" in
    Format.eprintf "TOTAL of  : %d@\n%!" (!count + 1);
    Hashtbl.iter (fun n i -> id_to_name.(i) <- n) name_to_id;
    let valves = ~%[] in
    List.iter (fun (s, c) -> valves.%[name_to_id.%[s]] <- c) val_list;
    let edges = Array.make (!count + 1) [] in
    let valve_mask =
      let m = ref 0 in
      for i = 1 to Hashtbl.length valves do
        m := !m lor (1 lsl i)
      done;
      !m
    in
    List.iter
      (fun (s, (t, c)) ->
        let is = name_to_id.%[s] in
        let it = name_to_id.%[t] in
        edges.(is) <- (it, c) :: edges.(is))
      e_list;
    { edges; name_to_id; id_to_name; valves; valve_mask }

  let iter_succ g v f = List.iter f g.edges.(v)
  let fold_succ g v f acc = List.fold_left f acc g.edges.(v)

  let pp fmt g =
    iter_vertices g (fun v ->
        Format.fprintf fmt "%s :[%a]@\n%!" g.id_to_name.(v)
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             Format.pp_print_string)
          (List.map (fun (w, _) -> Array.get g.id_to_name w) g.edges.(v)))

  let fold_absent_valves g f acc vset =
    let rec loop i acc =
      if i < 1 then acc
      else if BitSet.not_mem i vset then loop (i - 1) (f acc i)
      else loop (i - 1) acc
    in
    loop (Hashtbl.length g.valves) acc
end

module Algo = Utils.GraphAlgo (Graph)

let bint = function false -> 0 | true -> 1

let pack_simple_info ni v vset elephant =
  vset land 0xffffffff
  lor ((v land 0xffff) lsl 32)
  lor ((ni land 0xff) lsl 48)
  lor (bint elephant lsl 56)

let unpack_vset info = info land 0xffffffff
let unpack_i info = (info lsr 48) land 0xff
let unpack_simple_node info = (info lsr 32) land 0xffff
let unpack_elephant info = (info lsr 56) land 1 != 0
let set_elephant info = info lor (1 lsl 56)

let all_simple_paths_opt n (g : Graph.t) dist root use_elephant =
  let open Graph in
  let cache = ~%[] in
  let all_valves = Hashtbl.fold (fun _ n acc -> n + acc) g.valves 0 in
  let global_max = ref 0 in
  let rec loop i info total vval =
    let vset = unpack_vset info in
    let elephant = unpack_elephant info in
    if i == 0 then begin
      if elephant || not use_elephant then begin
        global_max := max total !global_max;
        total
      end
      else loop n (pack_simple_info n root vset true) total vval
    end
    else
      try total + cache.%[info]
      with Not_found ->
        let v = unpack_simple_node info in
        let res =
          if
            total
            + (i + if use_elephant && not elephant then n else 0)
              * (all_valves - vval)
            < !global_max
          then total
          else
            fold_absent_valves g
              (fun acc w ->
                let vset = BitSet.add w vset in
                let d = dist.(v).(w) in
                let w_flow_rate = g.valves.%[w] in
                let d = max (i - d) 0 in
                let total = total + (d * w_flow_rate) in
                let ninfo = pack_simple_info d w vset elephant in
                let vval = vval + w_flow_rate in
                max acc (loop d ninfo total vval))
              total vset
        in
        cache.%[info] <- res - total;
        res
  in
  loop n (pack_simple_info n root BitSet.empty false) 0 0

let load_level () =
  let rates = ref [] in
  let edges = ref [] in
  Utils.fold_fields ' '
    (fun () -> function
      | "Valve" :: src :: "has" :: "flow" :: rate
        :: ("tunnels" | "tunnel")
        :: ("lead" | "leads")
        :: "to"
        :: ("valves" | "valve")
        :: valves ->
          let rate = Scanf.sscanf rate "rate=%d;" (fun x -> x) in
          if rate <> 0 then rates := (src, rate) :: !rates;
          valves
          |> List.iter (fun s ->
                 match String.split_on_char ',' (String.trim s) with
                 | dst :: _ -> begin
                     edges := (src, (dst, 1)) :: (dst, (src, 1)) :: !edges
                   end
                 | _ -> assert false)
      | _ -> ())
    ();
  let rates = List.sort compare !rates in
  let () = assert (List.length rates < 32) in
  let edges = List.sort_uniq compare !edges in
  let edges =
    List.fold_left (fun acc (s, _) -> (s, (s ^ "o", 1)) :: acc) edges rates
  in
  let nrates = List.map (fun (s, r) -> s ^ "o", r) rates in
  let g = Graph.build edges nrates in
  let () =
    Format.eprintf "VALVES HAVE ID: ";
    Hashtbl.iter (fun n _ -> if n <> 0 then Format.eprintf "%d " n) g.valves;
    Format.eprintf "@\n%!"
  in
  let () =
    List.iter
      (fun (s, _) ->
        let sop_id = g.name_to_id.%[s ^ "o"] in
        g.edges.(sop_id) <-
          List.filter (fun (id, _) -> id <> sop_id) g.edges.(g.name_to_id.%[s]))
      rates
  in
  g

let solve len use_elephant =
  let g = load_level () in
  let dist = Algo.floyd_warshall g in
  let dist_array =
    let len = Array.length g.id_to_name in
    Array.make_matrix len len max_int
  in
  let () = Hashtbl.iter (fun (i, j) d -> dist_array.(i).(j) <- d) dist in
  let t0 = Unix.gettimeofday () in
  let n =
    all_simple_paths_opt len g dist_array g.name_to_id.%["AA"] use_elephant
  in
  let t1 = Unix.gettimeofday () in
  Format.printf "%d (in %.4fms)@\n" n (1000. *. (t1 -. t0))

module Sol = struct
  let name = "16"
  let solve_part1 () = solve 30 false
  let solve_part2 () = solve 26 true
end

let () = Solution.register_mod (module Sol)