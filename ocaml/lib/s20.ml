type cell = { mutable left : cell; mutable right : cell; mutable value : int }

let chain cl =
  let rec loop cl =
    match cl with
    | [] -> assert false
    | [ last ] -> last
    | c1 :: (c2 :: _ as cll) ->
        c1.right <- c2;
        c2.left <- c1;
        loop cll
  in
  match cl with
  | [] -> assert false
  | first :: _ ->
      let last = loop cl in
      first.left <- last;
      last.right <- first;
      cl

let single value =
  let rec left = { left; right = left; value } in
  left

let move cell value =
  let rec loop dir n =
    if n > 0 then
      let () =
        if dir then begin
          let tmp = cell.right in
          cell.right <- tmp.right;
          cell.right.left <- cell;
          tmp.left <- cell.left;
          tmp.left.right <- tmp;
          tmp.right <- cell;
          cell.left <- tmp
        end
        else begin
          let tmp = cell.left in
          cell.left <- tmp.left;
          cell.left.right <- cell;
          tmp.right <- cell.right;
          tmp.right.left <- tmp;
          tmp.left <- cell;
          cell.right <- tmp
        end
      in
      loop dir (n - 1)
  in
  if value != 0 then loop (value > 0) (abs value)

let pp fmt cell =
  let rec loop rcell =
    if rcell != cell then Format.fprintf fmt ", ";
    Format.fprintf fmt "%d" rcell.value;
    let rcell = rcell.right in
    if rcell != cell then loop rcell
  in
  loop cell

let load_cells () =
  Utils.fold_lines (fun acc s -> single (int_of_string s) :: acc) []
  |> List.rev |> chain

let find_at_pos cell len n =
  let n = n mod len in
  let rec loop cell n = if n = 0 then cell.value else loop cell.right (n - 1) in
  loop cell n

let solve f =
  let cells = load_cells () in
  let len = List.length cells in
  let () = f len cells in
  let cell0 = List.find (fun c -> c.value = 0) cells in
  let res =
    List.fold_left
      (fun acc n -> acc + find_at_pos cell0 len n)
      0 [ 1000; 2000; 3000 ]
  in
  Format.printf "%d@\n%!" res

module Sol = struct
  let name = "20"

  let solve_part1 () =
    solve (fun _ cells -> List.iter (fun cell -> move cell cell.value) cells)

  let solve_part2 () =
    solve (fun len cells ->
        List.iter (fun cell -> cell.value <- 811589153 * cell.value) cells;
        for _ = 1 to 10 do
          List.iter (fun cell -> move cell (cell.value mod (len - 1))) cells
        done)
end

let () = Solution.register_mod (module Sol)