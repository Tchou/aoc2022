let overlap (a, b) (c, d) = (a >= c && b <= d) || (c >= a && d <= b)

let parse_interval s =
  match String.split_on_char '-' s with
  | [ s1; s2 ] -> (int_of_string s1, int_of_string s2)
  | _ -> failwith ("Invalid interval: " ^ s)

let solve test_interval () =
  let rec loop total =
    match String.split_on_char ',' (read_line ()) with
    | [ s1; s2 ] ->
        let p1 = parse_interval s1 in
        let p2 = parse_interval s2 in
        loop (total + if test_interval p1 p2 then 1 else 0)
    | _ -> loop total
    | exception End_of_file -> total
  in
  Printf.printf "%d\n" (loop 0)

let name = "04a"

let () = Solution.register name (solve overlap)


let intersect (a, b) (c, d) =
  let _, b, c, _ =
    if a < c then a, b, c, d else c, d, a, b in
  b >= c

let name = "04b"

let () = Solution.register name (solve intersect)


