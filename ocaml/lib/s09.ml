type config = {
  grid : (int * int, unit) Hashtbl.t;
  mutable rope : (int * int) list;
}

let mk_list_n n e =
  let rec loop n acc = if n = 0 then acc else loop (n - 1) (e :: acc) in
  loop n []

let make_config ?(orig = (0, 0)) n =
  assert (n >= 2);
  let init = orig in
  let grid = Hashtbl.create 16 in
  Hashtbl.replace grid init ();
  { grid; rope = mk_list_n n init }

let up = (-1, 0)
let down = (1, 0)
let right = (0, 1)
let left = (0, -1)
let ( ++ ) (a, b) (c, d) = (a + c, b + d)
let dist (r1, c1) (r2, c2) = (abs (r1 - r2), abs (c1 - c2))

let is_touching head tail =
  let d1, d2 = dist head tail in
  d1 <= 1 && d2 <= 1

let closer head tail =
  let rh, ch = head in
  let rt, ct = tail in
  let rs = rh - rt in
  let rs = if rs = 0 then 0 else rs / abs rs in
  let cs = ch - ct in
  let cs = if cs = 0 then 0 else cs / abs cs in
  (rs, cs)

let touch_grid grid pos = Hashtbl.replace grid pos ()

let pp_config row_num col_num fmt config =
  let lm =
    List.mapi
      (fun i pos -> (pos, if i = 0 then 'H' else Char.chr (i + Char.code '0')))
      config.rope
  in
  for row = 0 to row_num - 1 do
    for col = 0 to col_num - 1 do
      let c =
        try List.assoc (row, col) lm
        with Not_found ->
          if Hashtbl.mem config.grid (row, col) then '#' else '.'
      in
      Format.fprintf fmt "%c" c
    done;
    Format.fprintf fmt "\n"
  done

let move head tail =
  let d1, d2 = dist head tail in
  if d1 <= 1 && d2 <= 1 then tail else tail ++ closer head tail

let move_rope config dir =
  let rec loop rope front =
    match rope with
    | [] ->
        touch_grid config.grid front;
        []
    | tail :: rrope ->
        let ntail = move front tail in
        ntail :: loop rrope ntail
  in
  match config.rope with
  | [] -> assert false
  | head :: rrope ->
      let nhead = head ++ dir in
      config.rope <- nhead :: loop rrope nhead

let rec move_n sdir config dir n =
  if n > 0 then begin
    move_rope config dir;
    move_n sdir config dir (n - 1)
  end

let dir_of_string = function
  | "U" -> up
  | "D" -> down
  | "L" -> left
  | "R" -> right
  | _ -> assert false

let solve n () =
  let config = make_config n in
  Utils.fold_fields ' '
    (fun () -> function
      | [ sdir; sn ] ->
          let d = dir_of_string sdir in
          let n = int_of_string sn in
          move_n sdir config d n
      | _ -> ())
    ();
  let r = Hashtbl.length config.grid in
  Printf.printf "%d\n" r

let name = "09_part1"
let () = Solution.register name (solve 2)
let name = "09_part2"
let () = Solution.register name (solve 10)


(** Longer code to animate the rope in the terminal *)
let animate n () =
  let rec loop acc r c max_r max_c min_r min_c =
    match String.split_on_char ' ' (read_line ()) with
    | [ sdir; sn ] ->
        let d = dir_of_string sdir in
        let n = int_of_string sn in
        let acc = (d, n) :: acc in
        let nr, nc = (r, c) ++ d in
        loop acc nr nc (max max_r nr) (max max_c nc) (min min_r nr)
          (min min_c nc)
    | _ -> loop acc r c max_r max_c min_r min_c
    | exception End_of_file ->
        let num_row = abs (max_r - min_r) in
        let num_col = abs (max_c - min_c) in

        (List.rev acc, (num_row, num_col), 2 * num_row, 2 * num_col)
  in
  let cmd, orig, num_row, num_col = loop [] 0 0 0 0 0 0 in
  let config = make_config ~orig n in
  let _ =
    List.fold_left
      (fun config (d, n) ->
        Format.printf "%s%!" Utils.clear_screen;
        Format.printf "%a@\n%!"
          (pp_config num_row num_col)
          config;
        move_n "" config d n;
        Unix.sleepf 0.125;
        config)
      config cmd
  in
  Format.printf "%d@\n" (Hashtbl.length config.grid)

let name = "09_part2_animate"
let () = Solution.register name (animate 10)
