let process_present a f s =
  for i = 0 to String.length s - 1 do
    let idx = Char.code s.[i] in
    a.(idx) <- f a.(idx)
  done;
  a

let mark_present s = process_present (Array.make 256 0) (fun _ -> 1) s

exception Found of char

let find_present value s map =
  try
    for i = 0 to String.length s - 1 do
      let c = s.[i] in
      let idx = Char.code c in
      if map.(idx) = value then raise (Found c)
    done;
    None
  with Found c -> Some c

let priority c =
  assert ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
  if c >= 'a' then Char.code c - Char.code 'a' + 1
  else Char.code c - Char.code 'A' + 27

let solve () =
  let rec loop total =
    match read_line () with
    | s -> (
        let l = String.length s in
        let bag1 = String.sub s 0 (l / 2) in
        let bag2 = String.sub s (l / 2) (l / 2) in
        let map = mark_present bag1 in
        match find_present 1 bag2 map with
        | None -> loop total
        | Some c -> loop (total + priority c))
    | exception End_of_file -> total
  in
  Printf.printf "%d\n" (loop 0)

let name = "03a"
let () = Solution.register name solve

let print_array fmt a =
  for i = 0 to Array.length a - 1 do
    let v = a.(i) in
    if v <> 0 then Format.fprintf fmt "%C(%d)" (Char.chr i) a.(i)
  done

let solve () =
  let rec loop total =
    match
      let l1 = read_line () in
      let l2 = read_line () in
      let l3 = read_line () in
      (l1, l2, l3)
    with
    | l1, l2, l3 -> (
        let map1 = mark_present l1 in
        let map2 = mark_present l2 in
        let map3 = mark_present l3 in
        match
          try
            for i = 0 to 255 do
              if map1.(i) > 0 && map2.(i) > 0 && map3.(i) > 0 then
                raise (Found (Char.chr i))
            done;
            None
          with Found c -> Some c
        with
        | None -> assert false
        | Some c ->
            
            loop (total + priority c))
    | exception End_of_file -> total
  in
  Printf.printf "%d\n" (loop 0)

let name = "03b"
let () = Solution.register name solve
