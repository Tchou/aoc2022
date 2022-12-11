type command = Cd_root | Cd_up | Cd of string | Ls
type entry = Dir of string * entry list | File of string * int

let parse_cmd = function
  | [ "cd"; "/" ] -> Cd_root
  | [ "cd"; ".." ] -> Cd_up
  | [ "cd"; name ] -> Cd name
  | [ "ls" ] -> Ls
  | _ -> assert false

let rec find_entry entries dirname =
  match entries with
  | [] -> ([], Dir (dirname, []))
  | Dir (dn, files) :: rest when dn = dirname -> (rest, Dir (dn, files))
  | p :: rest ->
      let others, d = find_entry rest dirname in
      (p :: others, d)

let compare_entries a b =
  match (a, b) with
  | File (f1, s1), File (f2, s2) ->
      let c = compare f1 f2 in
      if c = 0 then assert (s1 = s2);
      c
  | Dir (d1, _), Dir (d2, _) -> compare d1 d2
  | File _, Dir _ -> -1
  | Dir _, File _ -> 1

let rec merge_entries e1 e2 =
  let e1 = List.sort compare_entries e1 in
  let e2 = List.sort compare_entries e2 in
  let rec loop l1 l2 =
    match (l1, l2) with
    | [], _ -> l2
    | _, [] -> l1
    | Dir (d1, ee1) :: ll1, Dir (d2, ee2) :: ll2 when d1 = d2 ->
        Dir (d1, merge_entries ee1 ee2) :: loop ll1 ll2
    | e1 :: ll1, e2 :: ll2 ->
        let c = compare_entries e1 e2 in
        if c = 0 then e1 :: loop ll1 ll2
        else if c < 0 then e1 :: loop ll1 l2
        else e2 :: loop l1 ll2
  in
  loop e1 e2

let rec apply_path d p =
  match (d, p) with
  | Dir (dirname, content), (dn2, entries) :: pp ->
      let others, dd = find_entry content dn2 in
      begin
        match dd with
        | Dir (_, entries2) ->
            let nentries = merge_entries entries entries2 in
            let ndd = Dir (dn2, nentries) in
            let nndd = apply_path ndd pp in
            Dir (dirname, nndd :: others)
        | File _ -> assert false
      end
  | _, [] -> d
  | File (n, _), _ -> failwith (Format.sprintf "Tries to cd into file %s" n)

let get_name = function Dir (n, _) | File (n, _) -> n
let sort_by_name l = List.sort (fun a b -> compare (get_name a) (get_name b)) l

let rec pp_tree fmt t =
  match t with
  | Dir (n, entries) ->
      let entries = sort_by_name entries in
      Format.fprintf fmt "- %s (dir)@[@\n" n;
      List.iter (fun e -> Format.fprintf fmt "%a" pp_tree e) entries;
      Format.fprintf fmt "@]@\n"
  | File (n, s) -> Format.fprintf fmt "@[- %s (size=%d)@]@\n" n s

let iter_on_dir_size f t =
  let rec loop t =
    match t with
    | File (_, s) -> s
    | Dir (_, entries) ->
        let size = List.fold_left (fun acc e -> acc + loop e) 0 entries in
        f size;
        size
  in
  ignore (loop t)

let sum_size limit t =
  let acc = ref 0 in
  let f s = if s <= limit then acc := !acc + s in
  iter_on_dir_size f t;
  !acc

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
                  (dirname, File (name, int_of_string ssize) :: entries)
                in
                (ndir :: path) :: ppaths
            | _ -> assert false
          end
        | _ -> assert false)
      [ [ ("/", []) ] ]
  in
  let paths = List.map List.rev (List.rev paths) in
  let t = List.fold_left apply_path (Dir ("#", [ Dir ("/", []) ])) paths in
  let t = match t with Dir ("#", [ e ]) -> e | _ -> assert false in
  Format.printf "%d@\n" (compute t)

let name = "07_part1"
let () = Solution.register name (solve (sum_size 100000))

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

let name = "07_part2"
let () = Solution.register name (solve (find_dir 70000000 30000000))
