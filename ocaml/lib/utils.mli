val fold_lines : ('a -> string -> 'a) -> 'a -> 'a
val fold_fields : char -> ('a -> string list -> 'a) -> 'a -> 'a
val fold_chars : ('a -> char -> 'a) -> 'a -> 'a
val clear_screen : string

module Syntax : sig
  module Hashtbl : sig
    type delete = Delete

    val ( ~% ) : ('a * 'b) list -> ('a, 'b) Hashtbl.t
    val ( .%[] ) : ('a, 'b) Hashtbl.t -> 'a -> 'b
    val ( .%[]<- ) : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
    val ( .%?[] ) : ('a, 'b) Hashtbl.t -> 'a -> bool
    val ( .%*[]<- ) : ('a, 'b) Hashtbl.t -> 'a -> delete -> unit
  end
end