let replace_min a v =
  let rec loop i len =
    if i < len then if a.(i) < v then a.(i) <- v else loop (i + 1) len
  in
  Array.sort compare a;
  loop 0 (Array.length a)

let solve len () =
  let arr_max = Array.make len 0 in
  let rec loop current_sum =
    match read_line () with
    | "" ->
        replace_min arr_max current_sum;
        loop 0
    | i -> loop (current_sum + int_of_string i)
    | exception _ -> Array.fold_left ( + ) 0 arr_max
  in
  Printf.printf "%d\n" (loop 0)

let name = "01a"
let () = Solution.register name (solve 1)
let name = "01b"
let () = Solution.register name (solve 3)
