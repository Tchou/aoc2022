let rec fold_lines f acc =
  match read_line () with
  | s ->
      let acc = f acc s in
      fold_lines f acc
  | exception End_of_file -> acc

let rec fold_fields sep f acc =
  match String.split_on_char sep (read_line ()) with
  | l ->
      let acc = f acc l in
      fold_fields sep f acc
  | exception End_of_file -> acc

let rec fold_chars f acc =
  match input_char stdin with
  | c -> fold_chars f (f acc c)
  | exception End_of_file -> acc

let clear_screen = "\x1b[1;1H\x1b[2J"

module Syntax = struct
  module Hashtbl = struct
    type delete = Delete

    let ( ~% ) l =
      let h = Hashtbl.create 16 in
      List.iter (fun (a, b) -> Hashtbl.add h a b) l;
      h

    let ( .%[]<- ) h k v = Hashtbl.replace h k v
    let ( .%[] ) h k = Hashtbl.find h k
    let ( .%?[] ) h k = Hashtbl.mem h k
    let ( .%*[]<- ) h k Delete = Hashtbl.remove h k
  end
end