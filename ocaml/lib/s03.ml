module Sol = struct
  let name = "03"

  let find_index a =
    let rec loop len i =
      if i >= len then raise Not_found else if a.(i) then i else loop len (i + 1)
    in
    loop (Array.length a) 0

  let find_unique l =
    l
    |> List.fold_left
         (fun (oldt, newt) s ->
           String.iter
             (fun c ->
               let c = Char.code c in
               newt.(c) <- newt.(c) || oldt.(c))
             s;
           Array.fill oldt 0 256 false;
           newt, oldt)
         (Array.make 256 true, Array.make 256 false)
    |> fst |> find_index |> Char.chr

  let priority c =
    assert ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'));
    Char.code c - if c >= 'a' then Char.code 'a' + 1 else Char.code 'A' + 27

  let solve_part1 () =
    Utils.fold_lines
      (fun total s ->
        let l = String.length s / 2 in
        total + priority (find_unique String.[ sub s 0 l; sub s l l ]))
      0
    |> Format.printf "%d\n"

  let solve_part2 () =
    let rec loop total =
      match
        let l1 = read_line () in
        let l2 = read_line () in
        let l3 = read_line () in
        [ l1; l2; l3 ]
      with
      | l -> loop (total + priority (find_unique l))
      | exception End_of_file -> total
    in
    Format.printf "%d\n" (loop 0)
end

let () = Solution.register_mod (module Sol)
