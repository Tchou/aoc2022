val register : string -> (unit -> unit) -> unit
val get : string -> (unit -> unit) option
val list : unit -> string list

module type S = sig
  val name : string
  val solve_part1 : unit -> unit
  val solve_part2 : unit -> unit
end

val register_mod : (module S) -> unit
