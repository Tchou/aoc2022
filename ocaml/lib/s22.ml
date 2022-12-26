open Utils.Syntax.Hashtbl

(*
For Part two the following approach is used :
- consider that your cube is a (playing) dice. Such a dice as the constraints
  that opposite faces add up to 7.
- this constraints make it so that for a given dice, when the orientation of two
  faces are known, then the others are fully determined.
- So first, create a map of a regular dice, this is Dice.faces below. In this
  table, for each face we write the number of the face left, below, right and
  above it.
- then we start with a DFS on the top-most left most face in the grid. We fix it
  arbitrarily to be face number 1 oriented with 3,2,4,5 left,below,right and top
  of it. Then during a dfs, we go left of the current face, right of the current
  face and below.
- Say we find a face below: We know the face we came from and we know it was
  above the current face. We use this information to realign the faces from our
  table (by doing a circular rotation of the array of the other faces until it
  matches). With that we can number each face and its neighbours:


        ...#               5
        .#..              314
        #...               2
        ....
...#.......#     6   1     1
........#...    453 532   324
..#....#....     1   6     6
..........#.               2     2
        ...#....          364   641
        .....#..           5     5
        .#......
        ......#.

- once this information is prepared, walking is easy. If we cross an edge,
  say the right of face 1, we know that we end up on face 4. Further we know
  we crossed from the right edge of 1 to arrive on the left edge of 4, since
  in face 4, the 1 is on the right. This tells us how to translate the coordinates
  from the starting face to the destination one.
*)
module Dice = struct
  let faces =
    [
      (*    L  D  R  U *)
      1, [| 3; 2; 4; 5 |];
      2, [| 3; 6; 4; 1 |];
      3, [| 1; 5; 6; 2 |];
      4, [| 6; 5; 1; 2 |];
      5, [| 4; 6; 3; 1 |];
      6, [| 3; 5; 4; 2 |];
    ]

  let index_of tab v =
    let rec loop len i =
      if i = len then raise Not_found
      else if tab.(i) = v then i
      else loop len (i + 1)
    in
    loop (Array.length tab) 0

  let align_faces tab v i =
    let res = Array.copy tab in
    let first = index_of tab v in
    let len = Array.length tab in
    for j = 0 to len - 1 do
      res.((i + j) mod len) <- tab.((first + j) mod len)
    done;
    res
end

module Edge = struct
  type t = Left | Bottom | Right | Top

  let to_int = function Left -> 0 | Bottom -> 1 | Right -> 2 | Top -> 3

  let of_int = function
    | 0 -> Left
    | 1 -> Bottom
    | 2 -> Right
    | 3 -> Top
    | _ -> assert false

  let pp fmt =
    let open Format in
    function
    | Left -> fprintf fmt "L"
    | Bottom -> fprintf fmt "B"
    | Right -> fprintf fmt "R"
    | Top -> fprintf fmt "T"
end

module Move = struct
  type t = Forward of int | Left | Right
end

let dirs = [ 0, 1; 1, 0; 0, -1; -1, 0 ]

let char_of_idir = function
  | 0 -> '>'
  | 1 -> 'v'
  | 2 -> '<'
  | 3 -> '^'
  | _ -> assert false

let int_of_dir d =
  let rec loop i = function
    | p :: l -> if p = d then i else loop (i + 1) l
    | _ -> assert false
  in
  loop 0 dirs

type face = {
  id : int;
  adjacent : int array;
  pixels : ((int * int) * char) array array;
  coord : int * int;
}

let dummy_face = { coord = 0, 0; pixels = [||]; adjacent = [||]; id = -1 }
let pp_vect fmt a = Format.fprintf fmt "<%d,%d,%d>" a.(0) a.(1) a.(2)

type row = { start : int; length : int; row : Bytes.t }
type grid = { size : int; rows : row array; faces : (int, face) Hashtbl.t }

let fail k = Format.ksprintf (fun s -> failwith s) k

let bytes_get b i =
  if i < 0 || i >= Bytes.length b then
    fail "Bytes index out of bounds %d '%s'" i (Bytes.to_string b)
  else Bytes.get b i

let array_get b i =
  if i < 0 || i >= Array.length b then fail "Array index out of bounds %d" i
  else b.(i)

let string_get b i =
  if i < 0 || i >= String.length b then fail "String index out of bounds %d" i
  else b.[i]

let get grid (r, c) =
  assert (0 <= r && r < Array.length grid.rows);
  let row = array_get grid.rows r in
  (* assert (c - row.start >= 0 && c < row.start + row.length); *)
  bytes_get row.row (c - row.start)

let rec vertical_step2d grid len r c i =
  let nr = (r + i + len) mod len in
  let row = array_get grid nr in
  if c >= row.start && c < row.start + row.length then nr, c
  else vertical_step2d grid len nr c i

(** unit movement i / j in -1/0/1 *)
let step2d grid (r, c) ((i, j) as dir) =
  assert ((i == 0 || j == 0) && -1 <= i && i <= 1 && -1 <= j && j <= 1);
  if i == 0 then
    (* horizontal move *)
    let row = array_get grid.rows r in
    let c = c - row.start in
    (r, row.start + ((c + j + row.length) mod row.length)), dir
  else
    (* vertical move *)
    vertical_step2d grid.rows (Array.length grid.rows) r c i, dir


let dummy_pos = (-1, -1), (0, 0)

let pp_face_with_pos pos fmt f =
  Format.fprintf fmt "id: %d@\n" f.id;
  Format.fprintf fmt "adjacent:%s@\n"
    (String.concat ", "
       ((Array.mapi (fun i e ->
             Format.asprintf "%d(%a)" e Edge.pp Edge.(of_int i)))
          f.adjacent
       |> Array.to_list));
  Format.fprintf fmt "pixels:@\n";
  Array.iteri
    (fun i r ->
      Array.iteri
        (fun j (_, c) ->
          let c =
            let (a, b), dir = pos in
            if a = i && b = j then char_of_idir (int_of_dir dir) else c
          in
          Format.fprintf fmt "%c" c)
        r;
      Format.fprintf fmt "@\n")
    f.pixels

let pp_face fmt f = pp_face_with_pos dummy_pos fmt f

let prepare_faces g =
  let faces_by_coords = ~%[] in
  let () =
    for r = 0 to (Array.length g.rows / g.size) - 1 do
      let row = r * g.size in
      for c = 0 to (g.rows.(row).length / g.size) - 1 do
        let col = (c * g.size) + g.rows.(row).start in
        let pixels = Array.make_matrix g.size g.size ((0, 0), ' ') in
        for i = row to row + g.size - 1 do
          for j = col to col + g.size - 1 do
            pixels.(i - row).(j - col) <- (i, j), get g (i, j)
          done
        done;
        Format.eprintf "FOUND FACE: %d, %d@\n%!" row col;
        faces_by_coords.%[row, col] <- { dummy_face with pixels }
      done
    done
  in
  let rec loop_faces ((r, c) as coord) id parent par_edge =
    let open Edge in
    if faces_by_coords.%?[r, c] then begin
      match faces_by_coords.%[r, c] with
      | { adjacent = [||]; _ } ->
          let adjacent =
            Dice.align_faces (List.assoc id Dice.faces) parent (to_int par_edge)
          in
          let face = { (faces_by_coords.%[r, c]) with id; adjacent; coord } in
          faces_by_coords.%[r, c] <- face;
          g.faces.%[id] <- face;
          loop_faces (r, c - g.size) adjacent.(to_int Left) id Right;
          loop_faces (r, c + g.size) adjacent.(to_int Right) id Left;
          loop_faces (r + g.size, c) adjacent.(to_int Bottom) id Top
      | _ -> ()
    end
  in
  loop_faces (0, g.rows.(0).start) 1
    (List.assoc 1 Dice.faces).(Edge.(to_int Left))
    Edge.Left;
  faces_by_coords

let right (i, j) = j, -i
let left (i, j) = -j, i

let step3d grid (face, (r, c)) ((i, j) as dir) =
  let open Edge in
  let ((nr, nc) as npos) = r + i, c + j in
  if nr >= 0 && nr < grid.size && nc >= 0 && nc < grid.size then
    (face, npos), dir
  else
    let out_edge =
      if j > 0 then Right
      else if j < 0 then Left
      else if i > 0 then Bottom
      else Top
    in
    let nface_id = face.adjacent.(to_int out_edge) in
    let nface = grid.faces.%[nface_id] in
    let in_edge = of_int (Dice.index_of nface.adjacent face.id) in

    match out_edge, in_edge with
    | Left, Right | Right, Left -> (nface, (r, grid.size - c - 1)), dir
    | Top, Bottom | Bottom, Top -> (nface, (grid.size - r - 1, c)), dir
    | Top, Left | Bottom, Right -> (nface, (c, r)), right dir
    | Left, Top | Right, Bottom -> (nface, (c, r)), left dir
    | Right, Top | Left, Bottom ->
        (nface, (grid.size - c - 1, grid.size - r - 1)), right dir
    | Top, Right | Bottom, Left ->
        (nface, (grid.size - c - 1, grid.size - r - 1)), left dir
    | Bottom, Bottom | Top, Top ->
        (nface, (r, grid.size - c - 1)), left (left dir)
    | Right, Right | Left, Left ->
        (nface, (grid.size - r - 1, c)), left (left dir)

let p3d_to_2d (face, (row, col)) = fst face.pixels.(row).(col)

(* Main part *)

let dummy = ~%[]

let pp_grid ?(map = dummy) fmt grid =
  Array.iteri
    (fun r row ->
      Format.fprintf fmt "%s" (String.make row.start ' ');
      for i = 0 to row.length - 1 do
        let c = row.start + i in
        let chr, pre, post =
          try
            let dir, color = map.%[r, c] in
            ( char_of_idir (int_of_dir dir),
              Format.sprintf "\x1b[0;3%dm" color,
              "\x1b[0m" )
          with Not_found -> get grid (r, c), "", ""
        in
        Format.fprintf fmt "%s%c%s" pre chr post
      done;
      Format.fprintf fmt "@\n")
    grid.rows

let index_of s c = try String.index s c with Not_found -> String.length s
let isqrt i = int_of_float (sqrt (float i))

let load_level () =
  let accg, acco =
    Utils.fold_lines
      (fun (accg, acco) line ->
        if line = "" then accg, acco
        else
          let i = index_of line '.' in
          if i >= String.length line then
            (* order line *)
            ( accg,
              let buff = Buffer.create 16 in
              let () =
                line
                |> String.iter (function
                     | ('L' | 'R') as c ->
                         Buffer.add_char buff ';';
                         Buffer.add_char buff c;
                         Buffer.add_char buff ';'
                     | c -> Buffer.add_char buff c)
              in
              Buffer.contents buff |> String.split_on_char ';'
              |> List.filter_map (function
                   | "" -> None
                   | "L" -> Some Move.Left
                   | "R" -> Some Move.Right
                   | i -> Some (Move.Forward (int_of_string i))) )
          else
            (* grid *)
            let start = min (index_of line '.') (index_of line '#') in
            let line = String.trim line in
            ( { start; length = String.length line; row = Bytes.of_string line }
              :: accg,
              acco ))
      ([], [])
  in
  let rows = accg |> List.rev |> Array.of_list in
  let num_rows = Array.fold_left (fun acc row -> acc + row.length) 0 rows in
  let size = isqrt (num_rows / 6) in
  assert (size * size * 6 = num_rows);
  { rows; size; faces = ~%[] }, acco

let path = ~%[]
let color = ref 0

let forward step to2d grid pos dir n =
  let rec loop pos dir n =
    path.%[to2d pos] <- dir, !color;
    if n = 0 then pos, dir
    else
      let npos, ndir = step grid pos dir in
      let c = get grid (to2d npos) in
      if c = '#' then pos, dir else loop npos ndir (n - 1)
  in
  color := ((!color + 1) mod 5) + 1;
  loop pos dir n

let walk step init_pos to2d grid orders =
  let rec loop pos dir l =
    match l with
    | [] -> pos, dir
    | Move.Left :: ll -> loop pos (left dir) ll
    | Move.Right :: ll -> loop pos (right dir) ll
    | Move.Forward n :: ll ->
        let npos, ndir = forward step to2d grid pos dir n in
        loop npos ndir ll
  in
  loop init_pos (0, 1) orders

module Sol = struct
  let name = "22"

  let solve_part1 () =
    let grid, moves = load_level () in
    let (r, c), dir =
      walk step2d (0, grid.rows.(0).start) (fun x -> x) grid moves
    in
    let d = int_of_dir dir in
    Format.eprintf "%a@\n%!" (pp_grid ~map:path) grid;
    let res = (1000 * (1 + r)) + (4 * (1 + c)) + d in
    Format.printf "%d@\n%!" res

  let solve_part2 () =
    let grid, moves = load_level () in
    let _ = prepare_faces grid in
    let init = grid.faces.%[1], (0, 0) in
    let fpos, dir = walk step3d init p3d_to_2d grid moves in
    let r, c = p3d_to_2d fpos in
    let d = int_of_dir dir in
    Format.eprintf "%a@\n%!" (pp_grid ~map:path) grid;
    let res = (1000 * (1 + r)) + (4 * (1 + c)) + d in
    Format.printf "%d@\n%!" res
end

let () = Solution.register_mod (module Sol)
