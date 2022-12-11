(** Generic interation row/column -wise, forward/backward *)
let init_col_fwd row _a = row, 0

let init_col_bwd row a = row, Array.length a.(row) - 1

let fwd_col a (r, c) =
  if c >= Array.length a.(r) then None else Some ((r, c + 1), a.(r).(c))

let bwd_col a (r, c) = if c < 0 then None else Some ((r, c - 1), a.(r).(c))
let init_row_fwd col _a = 0, col
let init_row_bwd col a = Array.length a - 1, col

let fwd_row a (r, c) =
  if r >= Array.length a then None else Some ((r + 1, c), a.(r).(c))

let bwd_row a (r, c) = if r < 0 then None else Some ((r - 1, c), a.(r).(c))

(* Iterate a function in all directions *)
let iter_all_directions f tab =
  for col = 0 to Array.length tab.(0) - 1 do
    f tab fwd_row (init_row_fwd col);
    f tab bwd_row (init_row_bwd col)
  done;
  for row = 0 to Array.length tab - 1 do
    f tab fwd_col (init_col_fwd row);
    f tab bwd_col (init_col_bwd row)
  done

(* While iterating in a direction, keep track of the max
   To avoid counting a tree several times, we mark it by
   replacing it by its complement to min_int in the array.
   using -v does not work since a tree can be of height 0.
*)
let count_visible accu tab next init =
  let rec loop ((r, c) as pos) max_found sum =
    match next tab pos with
    | None -> sum
    | Some (npos, v) ->
        if v < 0 then loop npos (max max_found (v + min_int)) sum
        else if v > max_found then begin
          tab.(r).(c) <- v + min_int;
          loop npos v (sum + 1)
        end
        else loop npos max_found sum
  in
  accu := loop (init tab) ~-1 !accu

let all_count tab =
  let total = ref 0 in
  iter_all_directions (count_visible total) tab;
  !total

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

let all_scenic_scores tab =
  let tab_res = Array.map (fun a -> Array.make (Array.length a) []) tab in
  iter_all_directions (compute_scenic_score tab_res) tab;
  Array.fold_left
    (fun max_found a ->
      Array.fold_left
        (fun max_found l ->
          let p = List.fold_left ( * ) 1 l in
          if p > max_found then p else max_found)
        max_found a)
    0 tab_res

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

module Sol = struct
  let name = "08"
  let solve_part1 = solve all_count
  let solve_part2 = solve all_scenic_scores
end

let () = Solution.register_mod (module Sol)