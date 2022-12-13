let solve n () =
  let cache = Array.make 26 (-n) in
  let rec loop count i =
    if count == n then i
    else
      match input_char stdin with
      | 'a' .. 'z' as c ->
          let c_idx = Char.code c - Char.code 'a' in
          let last_c = cache.(c_idx) in
          let () = cache.(c_idx) <- i in
          if i - last_c > count then loop (count + 1) (i + 1)
          else loop (i - last_c) (i + 1)
      | _ -> i
      | exception End_of_file -> i
  in
  Printf.printf "%d\n" (loop 0 0)

module Sol = struct
  let name = "06"
  let solve_part1 = solve 4
  let solve_part2 = solve 14
end

let () = Solution.register_mod (module Sol)