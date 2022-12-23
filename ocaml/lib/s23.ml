open Utils.Syntax.Hashtbl

let directions =
  [|
    [ 0, -1; -1, -1; 1, -1 ];
    [ 0, 1; -1, 1; 1, 1 ];
    [ -1, 0; -1, -1; -1, 1 ];
    [ 1, 0; 1, -1; 1, 1 ];
  |]

let count_elves elves =
  let min_x = ref max_int in
  let max_x = ref min_int in
  let min_y = ref max_int in
  let max_y = ref min_int in
  let () =
    Hashtbl.iter
      (fun (x, y) _ ->
        min_x := min x !min_x;
        max_x := max x !max_x;
        min_y := min y !min_y;
        max_y := max y !max_y)
      elves
  in
  !min_x, !max_x, !min_y, !max_y

let dump_elves elves =
  let min_x, max_x, min_y, max_y = count_elves elves in
  for y = min_y - 2 to max_y + 2 do
    for x = min_x - 2 to max_x + 2 do
      if elves.%?[x, y] then Format.eprintf "#" else Format.eprintf "."
    done;
    Format.eprintf "@\n%!"
  done

let simulate elves n =
  let decisions = ~%[] in
  let rec find_dir n c (x, y) =
    if c = 4 then None
    else if
      List.exists
        (fun (i, j) -> elves.%?[x + i, y + j])
        directions.((n + c) mod 4)
    then find_dir n (c + 1) (x, y)
    else Some (List.hd directions.((n + c) mod 4))
  in
  let round_one n (x, y) =
    try
      for i = -1 to 1 do
        for j = -1 to 1 do
          if i != 0 || j != 0 then
            if elves.%?[x + i, y + j] then raise_notrace Exit
        done
      done;
      None
    with Exit -> find_dir n 0 (x, y)
  in
  let rec loop round =
    if round < n then begin
      Hashtbl.iter
        (fun (x, y) _ ->
          match round_one round (x, y) with
          | None -> ()
          | Some (i, j) ->
              decisions.%[x + i, y + j] <-
                (x, y) :: (try decisions.%[x + i, y + j] with Not_found -> []))
        elves;
      let no_move = ref true in
      Hashtbl.iter
        (fun (xi, yj) lst ->
          match lst with
          | [ (x, y) ] ->
              no_move := false;
              elves.%*[x, y] <- Delete;
              elves.%[xi, yj] <- ()
          | _ -> ())
        decisions;
      Hashtbl.clear decisions;
      if !no_move then round else loop (round + 1)
    end
    else -1
  in
  loop 0

let load_level () =
  let elves = ~%[] in
  let _ =
    Utils.fold_lines
      (fun y line ->
        for x = 0 to String.length line - 1 do
          if line.[x] = '#' then elves.%[x, y] <- ()
        done;
        y + 1)
      0
  in
  elves

module Sol = struct
  let name = "23"

  let solve_part1 () =
    let elves = load_level () in
    let _ = simulate elves 10 in
    let min_x, max_x, min_y, max_y = count_elves elves in
    let size = (max_x - min_x + 1) * (max_y - min_y + 1) in
    let empty = size - Hashtbl.length elves in
    Format.printf "%d@\n%!" empty

  let solve_part2 () =
    let elves = load_level () in
    let n = simulate elves max_int in
    Format.printf "%d@\n%!" (n + 1)
end

let () = Solution.register_mod (module Sol)