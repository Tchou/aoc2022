open Utils.Syntax.Hashtbl

(* 2d movements *)
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
  normal : int array; (* the normal unit vector, identifying the face *)
  transform : int array array list;
      (* the transform used for reaching the face from the first one *)
  pixels : ((int * int) * char) array array;
      (* array of pixels containing the original point and char *)
  rotation : int; (* number of times the face was rotated *)
  connected : int array list;
}

let dummy_face =
  { normal = [||]; transform = []; pixels = [||]; rotation = 0; connected = [] }

let pp_vect fmt a = Format.fprintf fmt "<%d,%d,%d>" a.(0) a.(1) a.(2)

type row = { start : int; length : int; row : Bytes.t }

type grid = {
  size : int;
  rows : row array;
  faces : (int array, face) Hashtbl.t;
      (*map3d : (int array, info3d) Hashtbl.t; *)
      (* maps topmost corner of a sizexsize square
                                         to the dice number of the face. *)
}

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
let step2d grid (r, c) (i, j) =
  assert ((i == 0 || j == 0) && -1 <= i && i <= 1 && -1 <= j && j <= 1);
  if i == 0 then
    (* horizontal move *)
    let row = array_get grid.rows r in
    let c = c - row.start in
    r, row.start + ((c + j + row.length) mod row.length)
  else
    (* vertical move *)
    vertical_step2d grid.rows (Array.length grid.rows) r c i

(* Based on a dice (-1) to have correct array indices. for each face we list the
   corresponding face we enter when leaving in that direction as well as the new
   direction on the 2D plane.

*)
module G3D = struct
  (* convert a row, col to x,y,z vector *)
  let to3d r c = [| c; r; 0 |]

  (* rotation of pi2 around axes*)
  let rxppi2 = [| [| 1; 0; 0 |]; [| 0; 0; -1 |]; [| 0; 1; 0 |] |]
  let rxmpi2 = [| [| 1; 0; 0 |]; [| 0; 0; 1 |]; [| 0; -1; 0 |] |]
  let ryppi2 = [| [| 0; 0; 1 |]; [| 0; 1; 0 |]; [| -1; 0; 0 |] |]
  let rympi2 = [| [| 0; 0; -1 |]; [| 0; 1; 0 |]; [| 1; 0; 0 |] |]
  let rzppi2 = [| [| 0; -1; 0 |]; [| 0; 1; 0 |]; [| 0; 0; 1 |] |]
  let rzmpi2 = [| [| 0; 1; 0 |]; [| 0; -1; 0 |]; [| 0; 0; 1 |] |]

  (* unit vectors, used for identifying the faces *)
  let pux = [| 1; 0; 0 |]
  let mux = [| -1; 0; 0 |]
  let puy = [| 0; 1; 0 |]
  let muy = [| 0; -1; 0 |]
  let puz = [| 0; 0; 1 |]
  let muz = [| 0; 0; -1 |]
  let order = [ puz; puy; mux; pux; muy; muz ]

  let dice v =
    let i = ref 0 in
    let _ = List.find (fun w -> incr i; w = v) order in
    !i

  let vdice i = List.nth order (i - 1)

  (* face relationships. For each face, give the one on the
  right, below, left, above
  *)
  let cube =
    ~%[
        puz, [| pux; puy; mux; muy |];
        muz, [| mux; muy; pux; puy |];
        puy, [| pux; muz; mux; puz |];
        muy, [| mux; puz; pux; muz |];
        pux, [| muz; puy; puz; muy |];
        mux, [| puz; puy; muz; muy |];
      ]

  let rotate_neighbors pos a =
    let b = Array.copy a in
    let i = if pos then -1 else 1 in
    let len = Array.length a in
    for j = 0 to len - 1 do
      b.(j) <- a.((j + i + len) mod len)
    done;
    b

  let fold_down = rxppi2
  let fold_up = rxmpi2
  let fold_right = rympi2
  let fold_left = ryppi2
  let identity = [| [| 1; 0; 0 |]; [| 0; 1; 0 |]; [| 0; 0; 1 |] |]

  let matrix_mult a b =
    let rows = Array.length a in
    let cols = Array.length b.(0) in
    let result = Array.make_matrix rows cols 0 in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        for k = 0 to Array.length b - 1 do
          result.(i).(j) <- result.(i).(j) + (a.(i).(k) * b.(k).(j))
        done
      done
    done;
    result

  let apply_transform vect l =
    (List.fold_left (fun acc m -> matrix_mult acc m) [| vect |] l).(0)
end

let rec apply_n f x n = if n = 0 then x else apply_n f (f x) (n - 1)
let rotate_point len pos (i, j) = if pos then j, len - 1 - i else len - 1 - j, i

let rotate_pixels pos pixels =
  let len = Array.length pixels in
  let res = Array.make_matrix len len pixels.(0).(0) in
  for i = 0 to len - 1 do
    for j = 0 to len - 1 do
      let ni, nj = rotate_point len pos (i, j) in
      res.(i).(j) <- pixels.(ni).(nj)
    done
  done;
  res

let pp_face ?pos fmt f =
  Format.fprintf fmt "normal: %a@\n" pp_vect f.normal;
  Format.fprintf fmt "dice: %d@\n" (G3D.dice f.normal);
  Format.fprintf fmt "rotation: %d@\n" (f.rotation mod 4);
  Format.fprintf fmt "RN: %s@\n"
    (G3D.cube.%[f.normal]
    |> (fun a ->
         apply_n (G3D.rotate_neighbors (f.rotation < 0)) a (abs f.rotation))
    |> Array.map (fun a -> string_of_int (G3D.dice a))
    |> Array.to_list |> String.concat ", ");
  Format.fprintf fmt "Connected:%s@\n"
    (String.concat ", "
       (List.map (fun i -> string_of_int (G3D.dice i)) f.connected));
  Format.fprintf fmt "pixels:@\n";
  Array.iteri
    (fun i r ->
      Array.iteri
        (fun j (_, c) ->
          let c =
            match pos with
            | Some ((a, b), dir) when a = i && b = j ->
                char_of_idir (int_of_dir dir)
            | _ -> c
          in
          Format.fprintf fmt "%c" c)
        r;
      Format.fprintf fmt "@\n")
    f.pixels
(*
    (apply_n (rotate_pixels (f.rotation > 0)) f.pixels (abs f.rotation)) *)

let right (i, j) = j, -i
let left (i, j) = -j, i

let step_on_cube g face_id (r, c) ((i, j) as dir) =
  let ((nr, nc) as npos) = r + i, c + j in
  let dest, npos, _todo, ndir =
    if nr < 0 || nr >= g.size || nc < 0 || nc >= g.size then
      (* rotate orthogonal faces *)
      let neighbors = G3D.cube.%[face_id] in
      let face = g.faces.%[face_id] in
      let di, dj =
        apply_n
          (if face.rotation < 0 then left else right)
          dir (abs face.rotation)
      in
      match
        apply_n
          (G3D.rotate_neighbors (face.rotation < 0))
          neighbors (abs face.rotation)
      with
      | [| fr; fd; fl; fu |] ->
          let to_rpos, to_rneg, dest =
            if di < 0 then (* cross top border *) fr, fl, fu
            else if di > 0 then (* cross bottom border *) fl, fr, fd
            else if dj < 0 then (* cross left border *) fu, fd, fl
            else (*cross right border *) fd, fu, fr
          in
          let () =
            Format.eprintf "SELECTING NEIBOURGH %d@\n%!" (G3D.dice dest)
          in
          let destf =
            try g.faces.%[dest]
            with Not_found ->
              Format.eprintf "%a NOT FOUND@\n%!" pp_vect dest;
              raise Not_found
          in
          let npos = (nr + g.size) mod g.size, (nc + g.size) mod g.size in
          let npos, ndir =
            if List.mem destf.normal face.connected then npos, dir
            else
              ( apply_n
                  (rotate_point g.size (destf.rotation < 0))
                  npos (abs destf.rotation),
                apply_n
                  (if destf.rotation < 0 then right else left)
                  dir (abs destf.rotation) )
          in
          dest, npos, [ to_rpos, true; to_rneg, false ], ndir
      | _ -> assert false
    else face_id, npos, [], dir
  in
  let dest_face = g.faces.%[dest] in
  let _, chr = dest_face.pixels.(fst npos).(snd npos) in
  if chr = '#' then None
  else
    (*
    let () =
      List.iter
        (fun (todo, pos) ->
          let ftodo = g.faces.%[todo] in
          g.faces.%[todo] <-
            {
              ftodo with
              (*pixels = rotate_pixels pos ftodo.pixels *)
              (*rotation = (ftodo.rotation + if pos then 1 else -1)*);
            })
        todo
    in*)
    Some (dest, npos, ndir)

let step_on_cube g face_id (r, c) ((i, j) as dir) =
  Format.eprintf "STEPPING FROM %d, %d by %d, %d ON FACE:@\n%!" r c i j;
  let face = g.faces.%[face_id] in
  let oi, oj =
    apply_n (if face.rotation > 0 then right else left) dir (abs face.rotation)
  in
  Format.eprintf "ORIGINAL DIR: %d, %d@\n" oi oj;
  Format.eprintf "%a@\n%!" (pp_face ~pos:((r, c), dir)) g.faces.%[face_id];
  (*Format.eprintf "ALL FACES:@\n%!";
  let l =
    Hashtbl.fold
      (fun _ face acc ->
        Format.asprintf "%a@\n" (pp_face ~pos:((-1, -1), (0, 0))) face :: acc)
      g.faces []
  in
  let l = List.map (String.split_on_char '\n') l in
  let n = List.fold_left (fun acc l -> max acc (List.length l)) 0 l in
  let l = ref l in
  for _ = 0 to n - 1 do
    l :=
      List.map
        (function
          | [] -> []
          | p :: l ->
              Format.eprintf "%s" p;
              Format.eprintf "%s" (String.make (20 - String.length p) ' ');
              l)
        !l;
    Format.eprintf "@\n%!"
  done; *)

  let res = step_on_cube g face_id (r, c) dir in
  let () =
    match res with
    | None -> Format.eprintf "STOPPED@\n%!"
    | Some (dest, npos, ndir) ->
        if dest <> face_id then Format.eprintf "CROSSING EDGE@\n%!";
        Format.eprintf "SUCCESS: %d, %d, DIR %d, %d ON FACE:@\n" (fst npos)
          (snd npos) (fst ndir) (snd ndir);
        Format.eprintf "%a@\n%!" (pp_face ~pos:(npos, ndir)) g.faces.%[dest]
  in
  Format.eprintf "@\n----@\n%!";
  res

let forward_on_cube g face_id pos dir n =
  let rec loop face_id pos dir i =
    if i = 0 then face_id, pos, dir
    else
      match step_on_cube g face_id pos dir with
      | None -> face_id, pos, dir
      | Some (nface_id, npos, ndir) -> loop nface_id npos ndir (i - 1)
  in
  loop face_id pos dir n

type move = Forward of int | Left | Right

let walk_on_cube g moves =
  let rec loop face_id pos dir l =
    match l with
    | [] -> face_id, pos, dir
    | Left :: ll -> loop face_id pos (left dir) ll
    | Right :: ll -> loop face_id pos (right dir) ll
    | Forward n :: ll ->
        let nface_id, npos, ndir = forward_on_cube g face_id pos dir n in
        loop nface_id npos ndir ll
  in
  loop G3D.puz (0, 0) (0, 1) moves

let init_faces g =
  let r_orig, c_orig = 0, g.rows.(0).start in
  let faces_by_coords = ~%[] in
  for r = 0 to (Array.length g.rows / g.size) - 1 do
    let row = r * g.size in
    for c = 0 to (g.rows.(row).length / g.size) - 1 do
      let col = (c * g.size) + g.rows.(row).start in
      let rdiff = (row - r_orig) / g.size in
      let cdiff = (col - c_orig) / g.size in
      let rf = rdiff * g.size in
      let cf = cdiff * g.size in
      let pixels = Array.make_matrix g.size g.size ((0, 0), ' ') in
      let () =
        for i = 0 to g.size - 1 do
          for j = 0 to g.size - 1 do
            pixels.(i).(j) <-
              ( (row + i, col + j),
                bytes_get g.rows.(row + i).row (col + j - g.rows.(row + 1).start)
              )
          done
        done
      in
      faces_by_coords.%[rf, cf] <- { dummy_face with pixels };
      Format.eprintf "Found face: %d, %d@\n" rf cf
    done
  done;
  let rec loop_faces (r, c) rotation acc =
    if faces_by_coords.%?[r, c] then begin
      match faces_by_coords.%[r, c] with
      | { transform = []; _ } as face ->
          Format.eprintf "Processing face %d, %d@\n%!" r c;
          let normal = G3D.apply_transform [| 0; 0; 1 |] acc in
          Format.eprintf "Found normal vector: %a@\n%!" pp_vect normal;
          g.faces.%[normal] <- { face with rotation; transform = acc; normal };
          faces_by_coords.%[r, c] <- g.faces.%[normal];

          loop_faces (r, c - g.size) (rotation + 1) (G3D.fold_left :: acc);
          loop_faces (r, c + g.size) (rotation - 1) (G3D.fold_right :: acc);
          loop_faces (r + g.size, c) rotation (G3D.fold_down :: acc)
      | _ -> ()
    end
  in
  loop_faces (0, 0) 0 [ G3D.identity ];
  Hashtbl.iter
    (fun (r, c) _ ->
      let connected =
        List.filter_map
          (fun pos ->
            if faces_by_coords.%?[pos] then Some faces_by_coords.%[pos].normal
            else None)
          [ r - g.size, c; r + g.size, c; r, c - g.size; r, c + g.size ]
      in
      g.faces.%[faces_by_coords.%[r, c].normal] <-
        { (g.faces.%[faces_by_coords.%[r, c].normal]) with connected })
    faces_by_coords;
  Format.eprintf "Final face data:@\n";
  Hashtbl.iter
    (fun _ f -> Format.eprintf "%a" (pp_face ~pos:((-1, -1), (0, 0))) f)
    g.faces

(* Main part *)

let dummy = ~%[]

let pp_grid ?(map = dummy) fmt grid =
  Array.iteri
    (fun r row ->
      Format.fprintf fmt "%s" (String.make row.start ' ');
      for i = 0 to row.length - 1 do
        let c = row.start + i in
        let chr =
          try char_of_idir (int_of_dir map.%[r, c])
          with Not_found -> get grid (r, c)
        in
        Format.fprintf fmt "%c" chr
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
                   | "L" -> Some Left
                   | "R" -> Some Right
                   | i -> Some (Forward (int_of_string i))) )
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
  Format.eprintf "SIZE: %d@\n%!" size;
  assert (size * size * 6 = num_rows);
  { rows; size; faces = ~%[] }, acco

let path = ~%[]

let forward step to2d grid pos dir n =
  let rec loop pos n =
    path.%[to2d pos] <- dir;
    if n = 0 then pos
    else
      let npos = step grid pos dir in
      let c = get grid (to2d npos) in
      if c = '#' then pos else loop npos (n - 1)
  in
  loop pos n

let walk step init_pos to2d grid orders =
  let rec loop pos dir l =
    match l with
    | [] -> pos, dir
    | Left :: ll -> loop pos (left dir) ll
    | Right :: ll -> loop pos (right dir) ll
    | Forward n :: ll -> loop (forward step to2d grid pos dir n) dir ll
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
    let () = init_faces grid in
    let face_id, (r, c), d = walk_on_cube grid moves in
    let (r, c), _ = grid.faces.%[face_id].pixels.(r).(c) in
    let d = int_of_dir d in
    Format.eprintf "%d, %d, %d, @\n%!" (r + 1) (c + 1) d;
    let res = (1000 * (1 + r)) + (4 * (1 + c)) + d in
    Format.printf "%d@\n%!" res
end

let () = Solution.register_mod (module Sol)
