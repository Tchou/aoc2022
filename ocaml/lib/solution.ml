let table = Hashtbl.create 16
let register (n : string) (solve : unit -> unit) = Hashtbl.replace table n solve
let get (n : string) = Hashtbl.find_opt table n
let list () = table |> Hashtbl.to_seq_keys |> List.of_seq |> List.sort compare

module type S = sig
  val name : string
  val solve_part1 : unit -> unit
  val solve_part2 : unit -> unit
end

let register_mod (module X : S) =
  register (X.name ^ "_part1") X.solve_part1;
  register (X.name ^ "_part2") X.solve_part2
