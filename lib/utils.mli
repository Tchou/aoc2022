val fold_lines : ('a -> string -> 'a) -> 'a -> 'a
val fold_fields : char -> ('a -> string list -> 'a) -> 'a -> 'a
val fold_chars : ('a -> char -> 'a) -> 'a -> 'a