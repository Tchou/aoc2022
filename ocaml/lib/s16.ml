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

  let length s =
    let rec loop c s = if s == 0 then c else loop (c + 1) (s land (s - 1)) in
    loop 0 s

  let pp len fmt s =
    for i = 1 to len do
      Format.fprintf fmt "%d" ((s lsr (len - i)) land 1)
    done
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

let pack_simple_info ni v vset =
  vset land 0xffffffff lor ((v land 0xffff) lsl 32) lor ((ni land 0xff) lsl 48)

let unpack_vset info = info land 0xffffffff
let unpack_i info = (info lsr 48) land 0xff
let unpack_simple_node info = (info lsr 32) land 0xffff

let all_simple_paths n (g : Graph.t) dist root =
  let open Graph in
  let max_for_vset = ~%[] in
  let global_max = ref 0 in
  let all_valves = Hashtbl.fold (fun _ n acc -> acc + n) g.valves 0 in
  let rec loop i info total vval =
    let vset = unpack_vset info in
    global_max := max total !global_max;
    max_for_vset.%[vset] <-
      max total (try max_for_vset.%[vset] with Not_found -> 0);
    if i == 0 then total
    else
      let v = unpack_simple_node info in
      let res =
        if total + (i * (all_valves - vval)) < !global_max then total
        else
          fold_absent_valves g
            (fun acc w ->
              let vset = BitSet.add w vset in
              let d = dist.(v).(w) in
              let w_flow_rate = g.valves.%[w] in
              let d = max (i - d) 0 in
              let total = total + (d * w_flow_rate) in
              let ninfo = pack_simple_info d w vset in
              let vval = vval + w_flow_rate in
              max acc (loop d ninfo total vval))
            total vset
      in
      res
  in
  let _ = loop n (pack_simple_info n root BitSet.empty) 0 0 in
  max_for_vset, !global_max

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

let prepare () =
  let g = load_level () in
  let dist = Algo.floyd_warshall g in
  let dist_array =
    let len = Array.length g.id_to_name in
    Array.make_matrix len len max_int
  in
  let () = Hashtbl.iter (fun (i, j) d -> dist_array.(i).(j) <- d) dist in
  g, dist_array

module Sol = struct
  let name = "16"

  let solve_part1 () =
    let g, dist_array = prepare () in
    let t0 = Unix.gettimeofday () in
    let _, n = all_simple_paths 30 g dist_array g.name_to_id.%["AA"] in
    let t1 = Unix.gettimeofday () in
    Format.eprintf "%fms@\n%!" (1000. *. (t1 -. t0));
    Format.printf "%d@\n%!" n

  let solve_part2 () =
    let g, dist_array = prepare () in
    let t0 = Unix.gettimeofday () in
    let max_for_valves, _ =
      all_simple_paths 26 g dist_array g.name_to_id.%["AA"]
    in
    let max_cost = ref 0 in
    let entries = max_for_valves |> Hashtbl.to_seq |> Array.of_seq in
    let () =
      let all_bits = g.valve_mask lor 1 in
      for i = 0 to Array.length entries - 1 do
        let config1, n1 = entries.(i) in
        let vset1 = unpack_vset config1 in
        for j = i + 1 to Array.length entries - 1 do
          let config2, n2 = entries.(j) in
          let vset2 = unpack_vset config2 in
          if lnot vset1 lor lnot vset2 land all_bits = all_bits then
            max_cost := max !max_cost (n1 + n2)
        done
      done
    in
    let t1 = Unix.gettimeofday () in
    Format.eprintf "%fms@\n%!" (1000. *. (t1 -. t0));
    Format.printf "%d@\n%!" !max_cost
end

let () = Solution.register_mod (module Sol)