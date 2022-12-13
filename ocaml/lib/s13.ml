type packet = Int of int | List of packet list

let rec compare p1 p2 =
  match p1, p2 with
  | Int i1, Int i2 -> Stdlib.compare i1 i2
  | List p1, List p2 -> compare_list p1 p2
  | List p1, Int _ -> compare_list p1 [ p2 ]
  | Int _, List p2 -> compare_list [ p1 ] p2

and compare_list l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | _, [] -> 1
  | [], _ -> -1
  | p1 :: ll1, p2 :: ll2 ->
      let c = compare p1 p2 in
      if c = 0 then compare_list ll1 ll2 else c

let parse_packets () =
  let buff = Buffer.create 16 in
  let get_number () =
    if Buffer.length buff = 0 then []
    else begin
      let s = Buffer.contents buff in
      Buffer.clear buff;
      [ Int (int_of_string s) ]
    end
  in

  Utils.fold_chars
    (fun ((acc_l, current) as acc) -> function
      | '0' .. '9' as c -> Buffer.add_char buff c; acc
      | ',' -> acc_l, get_number () @ current
      | '[' -> List current :: acc_l, []
      | ']' ->
          let current = List (List.rev (get_number () @ current)) in
          begin
            match acc_l with
            | List l :: acc_l -> acc_l, current :: l
            | _ -> failwith "unbalanced ]"
          end
      | _ -> acc)
    ([], [])
  |> snd |> List.rev

let count_ordered packets =
  let rec loop i acc l =
    match l with
    | [] -> acc
    | [ _ ] -> assert false
    | p1 :: p2 :: ll ->
        loop (i + 1) (if compare p1 p2 < 0 then i + acc else acc) ll
  in
  loop 1 0 packets

let rec locate_from packet lp i =
  match lp with
  | [] -> raise Not_found
  | p :: llp ->
      let c = compare p packet in
      if c = 0 then i, llp
      else if c < 0 then locate_from packet llp (i + 1)
      else raise Not_found

let locate_dividers lp =
  let d1 = List [ List [ Int 2 ] ] in
  let d2 = List [ List [ Int 6 ] ] in
  let lp = d1 :: d2 :: lp in
  let lp = List.sort compare lp in
  let pd1, rest = locate_from d1 lp 1 in
  let pd2, _ = locate_from d2 rest (pd1 + 1) in
  pd1 * pd2

module Sol = struct
  let name = "13"

  let solve_part1 () =
    let packets = parse_packets () in
    let n = count_ordered packets in
    Format.printf "%d@\n" n

  let solve_part2 () =
    let packets = parse_packets () in
    let n = locate_dividers packets in
    Format.printf "%d@\n" n
end

let () = Solution.register_mod (module Sol)
