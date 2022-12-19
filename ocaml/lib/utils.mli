val fold_lines : ('a -> string -> 'a) -> 'a -> 'a
val fold_fields : char -> ('a -> string list -> 'a) -> 'a -> 'a
val fold_chars : ('a -> char -> 'a) -> 'a -> 'a
val clear_screen : string
val compare_fst : 'a * 'b -> 'a * 'b -> int
val compare_snd : 'a * 'b -> 'a * 'b -> int

module Syntax : sig
  module Hashtbl : sig
    module Hashtbl : sig
      include module type of Hashtbl

      val iter_all : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
    end

    type delete = Delete

    val ( ~% ) : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val ( .%[] ) : ('a, 'b) Hashtbl.t -> 'a -> 'b
    val ( .%[]<- ) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
    val ( .%![] ) : ('a, 'b) Hashtbl.t -> 'a -> 'b list
    val ( .%![]<- ) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
    val ( .%?[] ) : ('a, 'b) Hashtbl.t -> 'a -> bool
    val ( .%*[]<- ) : ('a, 'b) Hashtbl.t -> 'a -> delete -> unit
  end
end

module type GRAPH = sig
  type v
  type t

  val iter_vertices : t -> (v -> unit) -> unit
  val iter_succ : t -> v -> (v * int -> unit) -> unit
  val fold_succ : t -> v -> ('acc -> v * int -> 'acc) -> 'acc -> 'acc
end

module GraphAlgo (Graph : GRAPH) : sig
  val dijkstra : Graph.t -> Graph.v -> (Graph.v, int) Hashtbl.t -> unit
  val floyd_warshall : Graph.t -> (Graph.v * Graph.v, int) Hashtbl.t

  val reverse_floyd_warshall
    :  Graph.t -> (Graph.v * Graph.v, int) Hashtbl.t ->
    (Graph.v * Graph.v, Graph.v option array) Hashtbl.t
end