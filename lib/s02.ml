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

let solve () =
  let rec loop total =
    match String.split_on_char ' ' (read_line ()) with
    | [ op; my ] ->
        let c = compare (of_letter op) (of_letter my) in
        let nscore = if c = 0 then 3 else if c < 0 then 6 else 0 in
        let nscore = total + nscore + score (of_letter my) in
        loop nscore
    | _ -> loop total
    | exception End_of_file -> total
  in
  Printf.printf "%d\n" (loop 0)

let name = "02a"
let () = Solution.register name solve

type result = Draw | Win | Loose

let r_of_letter = function
  | "X" -> Loose
  | "Y" -> Draw
  | "Z" -> Win
  | s -> failwith ("Invalid result: '" ^ s ^ "'")

let decide m r =
  match r with
  | Win -> (
      match m with Rock -> Paper | Paper -> Scissors | Scissors -> Rock)
  | Draw -> m
  | Loose -> (
      match m with Paper -> Rock | Scissors -> Paper | Rock -> Scissors)

let solve () =
  let rec loop total =
    match String.split_on_char ' ' (read_line ()) with
    | [ op; my ] ->
        let mop = of_letter op in
        let res = r_of_letter my in
        let mmy = decide mop res in
        let c = compare mop mmy in
        let nscore = if c = 0 then 3 else if c < 0 then 6 else 0 in
        let nscore = total + nscore + score mmy in
        loop nscore
    | _ -> loop total
    | exception End_of_file -> total
  in
  Printf.printf "%d\n" (loop 0)

let name = "02b"
let () = Solution.register name solve
