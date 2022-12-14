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

let score_move op my = ((4 - (op - my)) mod 3 * 3) + 1 + my
let decide1 old_score op_move my_move = old_score + score_move op_move my_move

let solve decide =
  let total =
    Utils.fold_fields ' '
      (fun total -> function
        | [ op; my ] -> decide total (of_letter op) (of_letter my)
        | _ -> total)
      0
  in
  Printf.printf "%d\n" total

let decide2 old_score op_move result =
  let my_move = (op_move + result + 2) mod 3 in
  old_score + score_move op_move my_move

module Sol = struct
  let name = "02"
  let solve_part1 () = solve decide1
  let solve_part2 () = solve decide2
end

let () = Solution.register_mod (module Sol)
