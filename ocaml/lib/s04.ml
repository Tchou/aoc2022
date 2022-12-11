let overlap (a, b) (c, d) = (a >= c && b <= d) || (c >= a && d <= b)

let parse_interval s =
  match String.split_on_char '-' s with
  | [ s1; s2 ] -> (int_of_string s1, int_of_string s2)
  | _ -> failwith ("Invalid interval: " ^ s)

let solve test_interval =
  let res =
    Utils.fold_fields ','
      (fun total -> function
        | [ s1; s2 ] ->
            let p1 = parse_interval s1 in
            let p2 = parse_interval s2 in
            total + if test_interval p1 p2 then 1 else 0
        | _ -> total)
      0
  in
  Printf.printf "%d\n" res

let intersect (a, b) (c, d) = (a <= c && c <= b) || (c <= a && a <= d)

module Sol = struct
  let name = "04"
  let solve_part1 () = solve overlap
  let solve_part2 () = solve intersect
end

let () = Solution.register_mod (module Sol)
