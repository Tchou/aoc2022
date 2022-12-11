let has_char s c =
  try
    ignore (String.index s c);
    true
  with Not_found -> false

let load_level () =
  let a = ref [||] in
  let ensure s =
    let l = String.length s + 1 in
    assert (l mod 4 = 0);
    let la = Array.length !a in
    if la = 0 then a := Array.make (l / 4) [] else assert (la = l / 4)
  in
  let rec loop () =
    match read_line () with
    | "" -> Array.map List.rev !a
    | s when has_char s '[' ->
        ensure s;
        for i = 0 to Array.length !a - 1 do
          let c = s.[(4 * i) + 1] in
          if c <> ' ' then !a.(i) <- c :: !a.(i)
        done;
        loop ()
    | s ->
        ensure s;
        loop ()
  in
  loop ()

let move a i j =
  match a.(i) with
  | e :: l ->
      a.(i) <- l;
      a.(j) <- e :: a.(j)
  | _ -> assert false

let move_9000 a i j n =
  let i = i - 1 in
  let j = j - 1 in
  for _ = 0 to n - 1 do
    move a i j
  done

let print_array fmt a =
  let sa =
    Array.map (fun l -> String.concat "" (List.map (String.make 1) l)) a
  in
  let max_l = ref 0 in
  Array.iter (fun s -> max_l := max !max_l (String.length s)) sa;
  Array.iteri
    (fun i s ->
      let pref = String.make (!max_l - String.length s) ' ' in
      sa.(i) <- pref ^ s)
    sa;
  for i = 0 to !max_l - 1 do
    for j = 0 to Array.length sa - 1 do
      let c = sa.(j).[i] in
      if c = ' ' then Format.fprintf fmt "    "
      else Format.fprintf fmt "[%c] " c
    done;
    Format.fprintf fmt "\n"
  done;
  for j = 1 to Array.length sa do
    Format.fprintf fmt "% 2d  " j
  done;
  Format.fprintf fmt "\n"

let debug a s =
  Format.eprintf "%s%!" Utils.clear_screen;
  Format.eprintf "%s@\n" s;
  Format.eprintf "%a@\n" print_array a;
  Unix.sleepf 0.5

let solve do_n_move ?(animate = false) () =
  let a = load_level () in
  let () = if animate then debug a "" in
  Utils.fold_fields ' '
    (fun () -> function
      | [ "move"; sn; "from"; si; "to"; sj ] as order ->
          let n = int_of_string sn in
          let i = int_of_string si in
          let j = int_of_string sj in
          do_n_move a i j n;
          if animate then debug a (String.concat " " order)
      | _ -> ())
    ();
  let af = Array.map (function c :: _ -> String.make 1 c | _ -> "") a in
  let lf = Array.to_list af in
  let res = String.concat "" lf in
  Format.printf "%s\n%!" res

let name = "05_part1"
let () = Solution.register name (solve move_9000)
let name = "05_part1_animate"
let () = Solution.register name (solve ~animate:true move_9000)

let split_n l n =
  let rec loop n l acc =
    if n = 0 then (acc, l)
    else
      match l with p :: ll -> loop (n - 1) ll (p :: acc) | _ -> assert false
  in
  loop n l []

let move_9001 a i j n =
  let i = i - 1 in
  let j = j - 1 in
  let to_move, staying = split_n a.(i) n in
  let dest = List.rev_append to_move a.(j) in
  a.(i) <- staying;
  a.(j) <- dest

let name = "05_part2"
let () = Solution.register name (solve move_9001)
let name = "05_part2_animate"
let () = Solution.register name (solve ~animate:true move_9001)
