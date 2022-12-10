let replace_min a v =
  let rec loop i len =
    if i < len then if a.(i) < v then a.(i) <- v else loop (i + 1) len
  in
  Array.sort compare a;
  loop 0 (Array.length a)

let solve len () =
  let arr_max = Array.make len 0 in
  let _ =
    Utils.fold_lines
      (fun current_sum -> function
        | "" ->
            replace_min arr_max current_sum;
            0
        | i -> current_sum + int_of_string i)
      0
  in
  let sum = Array.fold_left ( + ) 0 arr_max in
  Printf.printf "%d\n" sum

let name = "01a"
let () = Solution.register name (solve 1)
let name = "01b"
let () = Solution.register name (solve 3)
