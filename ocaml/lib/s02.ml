type move = Rock | Paper | Scissors

let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let of_letter = function
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | s -> failwith ("Invalid move: '" ^ s ^ "'")

let compare m1 m2 =
  match (m1, m2) with
  | Rock, Paper | Scissors, Rock | Paper, Scissors -> -1
  | Paper, Rock | Rock, Scissors | Scissors, Paper -> 1
  | Rock, Rock | Scissors, Scissors | Paper, Paper -> 0

let decide1 old_score op_move my_move_s =
  let my_move = of_letter my_move_s in
  let c = compare op_move my_move in
  let points = if c = 0 then 3 else if c < 0 then 6 else 0 in
  old_score + points + score my_move

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

type result = Draw | Win | Loose

let r_of_letter = function
  | "X" -> Loose
  | "Y" -> Draw
  | "Z" -> Win
  | s -> failwith ("Invalid result: '" ^ s ^ "'")

let decide2 old_score op_move my_move_s =
  let r = r_of_letter my_move_s in
  let my_move =
    match r with
    | Win -> (
        match op_move with
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock)
    | Draw -> op_move
    | Loose -> (
        match op_move with
        | Paper -> Rock
        | Scissors -> Paper
        | Rock -> Scissors)
  in
  let c = compare op_move my_move in
  let points = if c = 0 then 3 else if c < 0 then 6 else 0 in
  old_score + points + score my_move

let name = "02_part2"
let () = Solution.register name (solve decide2)
