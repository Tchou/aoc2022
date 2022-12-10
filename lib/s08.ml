let count_visible tab next init set =
  let rec loop pos max_found sum =
    match next tab pos with
    | None -> sum
    | Some (npos, v) ->
        if v < 0 then loop npos (max max_found (-v)) sum
        else if v > max_found then begin
          set tab pos (-v);
          loop npos v (sum + 1)
        end
        else loop npos max_found sum
  in
  loop (init tab) 0 0

let set a (r, c) v = a.(r).(c) <- v
let init_col_fwd row _a = (row, 0)
let init_col_bwd row a = (row, Array.length a.(row) - 1)

let fwd_col a (r, c) =
  if c >= Array.length a.(r) then None else Some ((r, c + 1), a.(r).(c))

let bwd_col a (r, c) = if c < 0 then None else Some ((r, c - 1), a.(r).(c))
let init_row_fwd col _a = (0, col)
let init_row_bwd col a = (Array.length a - 1, col)

let fwd_row a (r, c) =
  if r >= Array.length a then None else Some ((r + 1, c), a.(r).(c))

let bwd_row a (r, c) = if r < 0 then None else Some ((r - 1, c), a.(r).(c))

let all_count a =
  let total = ref 0 in
  for col = 0 to Array.length a.(0) - 1 do
    total := !total + count_visible a fwd_row (init_row_fwd col) set
  done;
  for col = Array.length a.(0) - 1 downto 0 do
    total := !total + count_visible a bwd_row (init_row_bwd col) set
  done;
  for row = 0 to Array.length a - 1 do
    total := !total + count_visible a fwd_col (init_col_fwd row) set
  done;
  for row = Array.length a - 1 downto 0 do
    total := !total + count_visible a bwd_col (init_col_bwd row) set
  done;
  !total

let solve f () =
  let res =
    Utils.fold_lines
      (fun acc s ->
        (s |> String.to_seq |> Array.of_seq
        |> Array.map (fun x -> Char.code x - Char.code '0'))
        :: acc)
      []
  in
  let res = Array.of_list (List.rev res) in
  Printf.printf "%d\n" (f res)

let name = "08a"
let () = Solution.register name (solve all_count)

let dist_to_passed_elems passed_elems v =
  let rec loop l acc =
    match l with
    | [] -> acc
    | p :: ll -> if p >= v then 1 + acc else loop ll (acc + 1)
  in
  loop passed_elems 0

let compute_scenic_score tab_res tab next init =
  let rec loop ((r, c) as pos) passed_elems =
    match next tab pos with
    | None -> ()
    | Some (npos, v) ->
        tab_res.(r).(c) <-
          dist_to_passed_elems passed_elems v :: tab_res.(r).(c);
        loop npos (v :: passed_elems)
  in
  loop (init tab) []

let dump_scenic_scores tab_res tab =
  for row = 0 to Array.length tab - 1 do
    for col = 0 to Array.length tab.(row) - 1 do
      let v = tab.(row).(col) in
      let ss = tab_res.(row).(col) in
      Format.eprintf " %d (%d =" v (List.fold_left ( * ) 1 ss);
      List.iter (fun i -> Format.eprintf "%d " i) ss;
      Format.eprintf ")"
    done;
    Format.eprintf "@\n"
  done

let all_scenic_scores tab =
  let tab_res = Array.map (fun a -> Array.make (Array.length a) []) tab in
  for col = 0 to Array.length tab.(0) - 1 do
    compute_scenic_score tab_res tab fwd_row (init_row_fwd col)
  done;
  for col = Array.length tab.(0) - 1 downto 0 do
    compute_scenic_score tab_res tab bwd_row (init_row_bwd col)
  done;
  for row = 0 to Array.length tab - 1 do
    compute_scenic_score tab_res tab fwd_col (init_col_fwd row)
  done;
  for row = Array.length tab - 1 downto 0 do
    compute_scenic_score tab_res tab bwd_col (init_col_bwd row)
  done;
  Array.fold_left
    (fun max_found a ->
      Array.fold_left
        (fun max_found l ->
          let p = List.fold_left ( * ) 1 l in
          if p > max_found then p else max_found)
        max_found a)
    0 tab_res

let name = "08b"
let () = Solution.register name (solve all_scenic_scores)
