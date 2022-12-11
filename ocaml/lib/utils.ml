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
