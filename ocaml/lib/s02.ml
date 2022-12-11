let _ROCK = 0
let _PAPER = 1
let _SCISSORS = 2
let next n = (n + 1) mod 3
let prev n = (n + 2) mod 3

let of_letter = function
  | "A" | "X" -> _ROCK
  | "B" | "Y" -> _PAPER
  | "C" | "Z" -> _SCISSORS
  | s -> failwith ("Invalid move: '" ^ s ^ "'")

let score_move op my =
  1 + my + if next op = my then 6 else if op = my then 3 else 0

let decide1 old_score op_move my_move_s =
  let my_move = of_letter my_move_s in
  old_score + score_move op_move my_move

let solve decide () =
  let total =
    Utils.fold_fields ' '
      (fun total -> function
        | [ op; my ] -> decide total (of_letter op) my
        | _ -> total)
      0
  in
  Printf.printf "%d\n" total

let name = "02_part1"
let () = Solution.register name (solve decide1)

let decide2 old_score op_move my_move_s =
  let my_move =
    match my_move_s with
    | "X" -> prev op_move
    | "Y" -> op_move
    | "Z" -> next op_move
    | s -> failwith ("Invalid result: '" ^ s ^ "'")
  in
  old_score + score_move op_move my_move

let name = "02_part2"
let () = Solution.register name (solve decide2)
