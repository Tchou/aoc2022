type command = Cd_root | Cd_up | Cd of string | Ls

type entry = Dir of entries | File of int
and entries = (string * entry) list

let parse_cmd = function
  | [ "cd"; "/" ] -> Cd_root
  | [ "cd"; ".." ] -> Cd_up
  | [ "cd"; name ] -> Cd name
  | [ "ls" ] -> Ls
  | _ -> assert false

let rec find_entry entries dirname =
  match entries with
  | [] -> ([], Dir [])
  | (dn, Dir files) :: rest when dn = dirname -> (rest, Dir files)
  | p :: rest ->
      let others, d = find_entry rest dirname in
      (p :: others, d)

let rec merge_entries e1 e2 =
  let e1 = List.sort compare e1 in
  let e2 = List.sort compare e2 in
  let rec loop l1 l2 =
    match (l1, l2) with
    | [], _ -> l2
    | _, [] -> l1
    | (d1, Dir ee1) :: ll1, (d2, Dir ee2) :: ll2 when d1 = d2 ->
        (d1, Dir (merge_entries ee1 ee2)) :: loop ll1 ll2
    | e1 :: ll1, e2 :: ll2 ->
        let c = compare e1 e2 in
        if c = 0 then e1 :: loop ll1 ll2
        else if c < 0 then e1 :: loop ll1 l2
        else e2 :: loop l1 ll2
  in
  loop e1 e2

let rec apply_path d p =
  match (d, p) with
  | Dir content, (dn2, new_entries) :: pp ->
      let others, dd = find_entry content dn2 in
      begin
        match dd with
        | Dir old_entries ->
            let entries = merge_entries old_entries new_entries in
            let new_dir = apply_path (Dir entries) pp in
            Dir ((dn2, new_dir) :: others)
        | File _ -> assert false
      end
  | _, [] -> d
  | File _, _ -> failwith (Format.sprintf "Tries to cd into a file")

let rec pp_entry fmt t =
  match t with
  | Dir entries -> entries |> List.sort compare |> pp_tree fmt
  | File s -> Format.fprintf fmt "(size=%d)" s

and pp_tree fmt l =
  List.iter
    (fun (name, e) -> Format.fprintf fmt "@[- %s @[%a@]@\n" name pp_entry e)
    l

let iter_on_dir_size f t =
  let rec loop t =
    match t with
    | File s -> s
    | Dir entries ->
        let size = List.fold_left (fun acc (_, e) -> acc + loop e) 0 entries in
        f size;
        size
  in
  ignore (loop t)

let sum_size limit t =
  let acc = ref 0 in
  let f s = if s <= limit then acc := !acc + s in
  iter_on_dir_size f t;
  !acc

let find_dir total needed t =
  let acc = ref [] in
  let f size = acc := size :: !acc in
  iter_on_dir_size f t;
  match !acc with
  | root_size :: rest ->
      let available = total - root_size in
      assert (available < needed);
      let l = List.sort compare rest in
      List.find (fun s -> s + available >= needed) l
  | _ -> assert false

let solve compute () =
  let paths =
    Utils.fold_fields ' '
      (fun paths -> function
        | "$" :: l -> begin
            match (paths, parse_cmd l) with
            | _, Cd_root -> [ ("/", []) ] :: paths
            | (_ :: ppaths) :: _, Cd_up -> ppaths :: paths
            | path :: ppaths, Cd name -> ((name, []) :: path) :: ppaths
            | _, Ls -> paths
            | _ -> assert false
          end
        | "dir" :: _ -> paths (* nothing to do for dirs *)
        | [ ssize; name ] -> begin
            match paths with
            | ((dirname, entries) :: path) :: ppaths ->
                let ndir =
                  (dirname, (name, File (int_of_string ssize)) :: entries)
                in
                (ndir :: path) :: ppaths
            | _ -> assert false
          end
        | _ -> assert false)
      [ [ ("/", []) ] ]
  in
  let paths = List.map List.rev (List.rev paths) in
  let t = List.fold_left apply_path (Dir [ ("/", Dir []) ]) paths in
  let _, t = match t with Dir [ e ] -> e | _ -> assert false in
  Format.printf "%d@\n" (compute t)

module Sol = struct
  let name = "07"
  let solve_part1 = solve (sum_size 100000)
  let solve_part2 = solve (find_dir 70000000 30000000)
end

let () = Solution.register_mod (module Sol)