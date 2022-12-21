open Utils.Syntax.Hashtbl

module SSet = struct
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
    let rec loop len i acc =
      if i > len then acc
      else if SSet.not_mem i vset then loop len (i + 1) (f acc i)
      else loop len (i + 1) acc
    in
    loop (Hashtbl.length g.valves) 1 acc
end

module Algo = Utils.GraphAlgo (Graph)

let pack_simple_info ni v vset =
  vset land 0xffffffff lor ((v land 0xffff) lsl 32) lor ((ni land 0xff) lsl 48)

let unpack_vset info = info land 0xffffffff
let unpack_i info = (info lsr 48) land 0xff
let unpack_simple_node info = (info lsr 32) land 0xffff
let calls = ref 0
let hits = ref 0

let all_simple_paths_opt n (g : Graph.t) dist root =
  let open Graph in
  let cache = ~%[] in
  let rec loop i info vval total =
    (* Invariant: we are at the begining of minute i, the valve
       of the current node (if any) has just started.
     *)
    if i == n then total
    else
      let vset = unpack_vset info in
      if vset = g.valve_mask then loop n info vval (((n - i) * vval) + total)
      else
        let () = incr calls in
        try
          let res = total + cache.%[info] in
          incr hits; res
        with Not_found ->
          let v = unpack_simple_node info in
          let res =
            fold_absent_valves g
              (fun acc w ->
                let vset = SSet.add w vset in
                let d = dist.(v).(w) in
                let w_flow_rate = g.valves.%[w] in
                let d, target_flow =
                  if d + i > n then n - i, 0 else d, w_flow_rate
                in
                let ni = d + i in
                let total = (vval * d) + total + target_flow in
                let vval = vval + w_flow_rate in
                let ninfo = pack_simple_info ni w vset in
                max acc (loop ni ninfo vval total))
              total vset
          in
          cache.%[info] <- res - total;
          res
  in
  loop 1 (pack_simple_info 1 root SSet.empty) 0 0

let unpack_vset info = info land 0xffffffff
let _unpack_i info = (info lsr 48) land 0xff
let unpack_double_low_node info = (info lsr 32) land 0xff
let unpack_double_high_node info = (info lsr 40) land 0xff
let unpack_d1_diff info = (info lsr 56) land 0x3f

let pack_double_info i v1 v2 vset d1_diff =
  vset land 0xffffffff
  lor ((v2 land 0xff) lsl 32)
  lor ((v1 land 0xff) lsl 40)
  lor ((i land 0xff) lsl 48)
  lor ((d1_diff land 0x3f) lsl 56)

(* iterate in parallel *)
let all_double_paths_opt n (g : Graph.t) dist at_dist root =
  let open Graph in
  let cache = ~%[] in
  let max_found = ref 0 in
  let vset_found = ref false in
  let all_valves_rates = Hashtbl.fold (fun _ v acc -> v + acc) g.valves 0 in
  let rec jump_equal w1 w2 d vset i vval total =
    let vset, w1_flow_rate = SSet.add w1 vset, g.valves.%[w1] in
    let vset, w2_flow_rate =
      if g.valves.%?[w2] then
        if SSet.not_mem w2 vset then SSet.add w2 vset, g.valves.%[w2]
        else vset, 0
      else vset, 0
    in
    let d, target_flow1, target_flow2 =
      if d + i > n then n - i, 0, 0 else d, w1_flow_rate, w2_flow_rate
    in
    let w2_flow_rate = if w1 == w2 then 0 else w2_flow_rate in
    let total = (vval * d) + total + target_flow1 + target_flow2 in
    let vval = vval + w1_flow_rate + w2_flow_rate in
    let ni = d + i in
    let ninfo =
      if w1 < w2 then pack_double_info ni w1 w2 vset 0
      else pack_double_info ni w2 w1 vset 0
    in
    loop ni ninfo vval total
  and loop i info vval total =
    (* at minute i, the low node is a valve to open,
        the high node is any other node that we iterate.
    *)
    let vset = unpack_vset info in
    if i == n then begin
      if total > !max_found then begin
        max_found := total;
        vset_found := vset = g.valve_mask;
        Format.eprintf "New max found: %d (%d/%d)@\n%!" !max_found vset
          g.valve_mask
      end;
      total
    end
    else if vset = g.valve_mask then loop n info vval (((n - i) * vval) + total)
    else
      let () = incr calls in
      try
        let res = total + cache.%[info] in
        incr hits; res
      with Not_found ->
        let v1 = unpack_double_low_node info in
        let v2 = unpack_double_high_node info in
        let res =
          if total + ((n - i) * all_valves_rates) < !max_found then total
          else
            fold_absent_valves g
              (fun acc w1 ->
                let d1 = dist.(v1).(w1) in
                fold_absent_valves g
                  (fun acc w2 ->
                    (* try to go to w1, w2 *)
                    let d2 = dist.(v2).(w2) in
                    if d1 == d2 then
                      (* both can jump the same distance *)
                      max acc (jump_equal w1 w2 d1 vset i vval total)
                    else
                      (* can't jump at the same distance, jump to the smallest
                       and let the other one catch-up*)
                      let d1, d2, w1, w2, v1, v2 =
                        if d1 > d2 then d2, d1, w2, w1, v2, v1
                        else d1, d2, w1, w2, v1, v2
                      in
                      (* w1 is the closest one *)
                      (* find a node at distance d1 from v2 *)
                      match at_dist.%[v2, w2].(d1) with
                      | None ->
                          ignore d2;
                          ignore v1;
                          (* no such node, stay here for w2*) assert false
                      | Some nw2 ->
                          (*found one, jump to new2 instead and restart from there*)
                          max acc (jump_equal w1 nw2 d1 vset i vval total))
                  acc vset)
              total vset
        in
        cache.%[info] <- res - total;
        res
  in
  loop 1 (pack_simple_info 1 root SSet.empty) 0 0

(*
     d1     U1   e1  V1    f1    W1
------------|--------|-----------|-----------------------------------------

                 d2                    U2   e2      V2   f2   W2
---------------------------------------|-------------|---------|------------
 

(U1, U2)
(d2 - d1 + 1) U1 + U2, d1_diff = d2 - d1

(V1, V2)
vval * e2 + 


*)

(* iterate in parallel *)
let all_double_paths_opt2 n (g : Graph.t) dist _at_dist root =
  let open Graph in
  let cache = ~%[] in
  let max_found = ref 0 in
  let rec loop i ((_, v1, d1_diff, v2, vset) as info) vval total =
    if false then
      Format.eprintf
        "%sMINUTE: % 2d, v1=%s (ov1=%s) d1=%d, v2=%s (ov2=%s), total=%d, \
         vval=%d, vset={%s}@\n\
         %!"
        (String.make i ' ') i g.id_to_name.(v1) "" d1_diff g.id_to_name.(v2) ""
        total vval
        (String.concat ", "
           (g.valves |> Hashtbl.to_seq
           |> Seq.filter (fun (i, _) -> SSet.mem i vset)
           |> List.of_seq |> List.sort compare
           |> List.map (fun (i, c) ->
                  Format.sprintf "%s (%d)" g.id_to_name.(i) c)));
    if i == n then begin
      if total > !max_found then begin
        max_found := total;
        Format.eprintf "New max found: %d (%d)@\n%!" !max_found
          (Hashtbl.length cache)
      end;
      total
    end
    else if vset = g.valve_mask then loop n info vval (((n - i) * vval) + total)
    else
      let () = incr calls in
      try
        let res = total + cache.%[info] in
        incr hits; res
      with Not_found ->
        let res =
          fold_absent_valves g
            (fun acc w1 ->
              let d1 = dist.(v1).(w1) - d1_diff in
              let vset = SSet.add w1 vset in
              fold_absent_valves g
                (fun acc w2 ->
                  (* try to go to w1, w2 *)
                  let d2 = dist.(v2).(w2) in
                  let vset = SSet.add w2 vset in

                  let w1, w2, d1, d2, _v1, _v2 =
                    if d1 > d2 then w2, w1, d2, d1, v2, v1
                    else w1, w2, d1, d2, v1, v2
                  in
                  (* d1 closest, d2 furthest*)
                  let w1_flow_rate = g.valves.%[w1] in
                  let w2_flow_rate = if w1 == w2 then 0 else g.valves.%[w2] in
                  let d1, target_flow1 =
                    if d1 + i > n then n - i, 0 else d1, w1_flow_rate
                  in
                  let d2, target_flow2 =
                    if d2 + i > n then n - i, 0 else d2, w2_flow_rate
                  in
                  (*
                      spend d2 minutes in total :
                      the amount will be :
                      vval * d2 + w2_flow_rate + (d2 - d1) * w1_flow_rate
                    *)
                  let total =
                    total + (vval * d2) + target_flow2
                    + ((d2 - d1 + 1) * target_flow1)
                  in
                  let vval = vval + w1_flow_rate + w2_flow_rate in
                  let ni = d2 + i in
                  let ninfo = ni, w1, d2 - d1, w2, vset in
                  max acc (loop ni ninfo vval total))
                acc vset)
            total vset
        in
        cache.%[info] <- res - total;
        res
  in
  loop 1 (1, root, 0, root, SSet.empty) 0 0

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

module Sol = struct
  let name = "16"

  let solve_part1 () =
    let g = load_level () in
    let dist = Algo.floyd_warshall g in
    let dist_array =
      let len = Array.length g.id_to_name in
      Array.make_matrix len len max_int
    in
    let () = Hashtbl.iter (fun (i, j) d -> dist_array.(i).(j) <- d) dist in
    let t0 = Unix.gettimeofday () in
    let n = all_simple_paths_opt 30 g dist_array g.name_to_id.%["AA"] in
    let t1 = Unix.gettimeofday () in
    Format.printf "%d (in %.4fms)@\n" n (1000. *. (t1 -. t0))

  let solve_part2 () =
    let g = load_level () in
    let dist = Algo.floyd_warshall g in
    let dist_array =
      let len = Array.length g.id_to_name in
      Array.make_matrix len len max_int
    in
    let () =
      Hashtbl.iter
        (fun (i, j) d ->
          Format.eprintf "%s -> %s = %d@\n%!" g.id_to_name.(i) g.id_to_name.(j)
            d;
          dist_array.(i).(j) <- d)
        dist
    in
    let at_dist = Algo.reverse_floyd_warshall g dist in
    let t0 = Unix.gettimeofday () in
    let n = all_double_paths_opt2 26 g dist_array at_dist g.name_to_id.%["AA"] in
    let t1 = Unix.gettimeofday () in
    Format.printf "%d (in %.4fms)@\n" n (1000. *. (t1 -. t0))
end

let () = Solution.register_mod (module Sol)