module Grid = struct
  type t = { grid : bytes array; overflow : (int * int, char) Hashtbl.t }

  let _AIR = '.'
  let _ROCK = '#'
  let _SAND = 'o'
  let is_air c = c == _AIR
  let is_sand c = c == _SAND

  let create r c =
    assert (r > 0 && c > 0);
    {
      grid = Array.init r (fun _ -> Bytes.make c _AIR);
      overflow = Hashtbl.create 16;
    }

  let inside g (r, c) =
    r >= 0 && r < Array.length g.grid && c >= 0 && c < Bytes.length g.grid.(0)

  let is_valid g (r, c) = inside g (r, c) || r < Array.length g.grid + 2
  let ( ++ ) (a, b) (c, d) = a + c, b + d

  let iter_points g f ((r1, c1) as p1) ((r2, c2) as p2) =
    assert (r1 == r2 || c1 == c2);
    let (r1, c1), (r2, c2) = min p1 p2, max p1 p2 in
    let r0, c0 = r2 - r1, c2 - c1 in
    let r0 = if r0 == 0 then r0 else r0 / abs r0 in
    let c0 = if c0 == 0 then 0 else c0 / abs c0 in
    let rec loop (r, c) =
      if (r, c) <> (r2, c2) then begin
        f g (r, c);
        loop ((r, c) ++ (r0, c0))
      end
      else f g (r, c)
    in
    loop (r1, c1)

  let set g ((r, c) as p) v =
    assert (is_valid g p);
    if inside g p then Bytes.set g.grid.(r) c v
    else Hashtbl.replace g.overflow (r, c) v

  let get g ((r, c) as p) =
    assert (is_valid g p);
    if inside g p then Bytes.get g.grid.(r) c
    else if r = 1 + Array.length g.grid then _ROCK
    else try Hashtbl.find g.overflow p with Not_found -> _AIR

  let build max_r max_col lines =
    let g = create (max_r + 1) (max_col + 1) in
    List.iter
      (fun (p1, p2) -> iter_points g (fun g p -> set g p _ROCK) p1 p2)
      lines;
    g

  let moves = [ 1, 0; 1, -1; 1, 1 ]

  type result =
    | STOP of (int * int)
    | FALL of (int * int)
    | MOVE of (int * int)

  let clean_around g p =
    let above = [ -1, -1; -1, 0; -1, 1 ] in
    let can_remove p =
      List.for_all (fun d -> not (is_air (get g (p ++ d)))) above
    in
    List.iter
      (fun d ->
        let q = d ++ p in
        if can_remove q then Hashtbl.remove g.overflow q)
      moves

  let move_down ?(fall = false) g p =
    assert (is_sand (get g p));
    let rec loop dirs =
      match dirs with
      | [] -> STOP p
      | dir :: ddirs ->
          let dest = p ++ dir in
          if (not (inside g dest)) && not fall then FALL dest
          else if is_air (get g dest) then begin
            set g p _AIR; set g dest _SAND; MOVE dest
          end
          else loop ddirs
    in
    loop moves

  let pp fmt g_ =
    let g = g_.grid in
    let len = Bytes.length g.(0) in
    let left =
      Array.fold_left
        (fun acc b -> try min acc (Bytes.index b '#') with _ -> acc)
        len g
    in
    let left = max 0 (left - 3) in
    let len = len - left in
    Array.iter
      (fun b ->
        Format.fprintf fmt "%s@\n" (String.sub (Bytes.to_string b) left len))
      g;
    Format.fprintf fmt "overflow: %d@\n" (Hashtbl.length g_.overflow)
end

let load_level () =
  let lines = ref [] in
  let max_r = ref 0 in
  let max_c = ref 0 in
  let read_pair s = Scanf.sscanf s "%d,%d" (fun x1 x2 -> x1, x2) in
  let rec parse_line = function
    | s1 :: (s2 :: _ as l) ->
        let x1, y1 = read_pair s1 in
        let x2, y2 = read_pair s2 in
        max_r := max !max_r (max y1 y2);
        max_c := max !max_c (max x1 x2);
        lines := ((y1, x1), (y2, x2)) :: !lines;
        parse_line l
    | _ -> ()
  in
  Utils.fold_fields ' '
    (fun () l -> l |> List.filter (fun s -> s <> "->") |> parse_line)
    ();
  Grid.build !max_r !max_c !lines

let count_sand ?(fall = false) ?(animate = false) g start =
  let rec loop_grain c p =
    if animate then begin
      Format.eprintf "%s%!" Utils.clear_screen;
      Format.eprintf "%a%!" Grid.pp g;
      Format.eprintf "%d@\n%!" c;
      Unix.sleepf (0.125 /. 5.)
    end;
    match Grid.move_down ~fall g p with
    | MOVE np -> loop_grain c np
    | FALL _ -> true
    | STOP np -> Grid.clean_around g np; np = start
  in
  let rec loop c =
    Grid.set g start Grid._SAND;
    if loop_grain c start then c else loop (c + 1)
  in
  loop 0

module Sol = struct
  let name = "14"

  let solve_part1 () =
    let g = load_level () in
    let n = count_sand ~fall:false ~animate:false g (0, 500) in
    Format.printf "%d@\n" n

  let solve_part2 () =
    let g = load_level () in
    let n = count_sand ~fall:true ~animate:false g (0, 500) in
    Format.printf "%d@\n" (n + 1)
end

let () = Solution.register_mod (module Sol)
