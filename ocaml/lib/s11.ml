let () = Format.pp_set_margin Format.err_formatter 180

type monkey = {
  items : (int * int) list Queue.t;
  operation : int -> int -> int;
  divisor : int;
  dest_true : int;
  dest_false : int;
  mutable count : int;
}
(* General structure. An integer n is represented as a list (di,n)
   where operations on n are done modulo di.
   As a special case, when the reducer is not 1, the list is only
   (max_int, n), meaning exact computation.
   This could be made even more general in cases wher for all di,
   the multiplicative inverse with the reducer (3 in the problem) is
   defined.
   I don't bother since the problem does not require it.   
*)

let pp_number fmt l =
  Format.fprintf fmt "[ %a]"
    (Format.pp_print_list (fun fmt (div, n) ->
         if div = max_int then Format.fprintf fmt "(%d orig) " n
         else Format.fprintf fmt "(%d mod %d) " n div))
    (List.sort compare l)

let pp_monkey fmt monkey =
  Format.fprintf fmt " DIVISOR: %d@\n" monkey.divisor;
  Format.fprintf fmt " DESTINATION ? %d : %d@\n" monkey.dest_true
    monkey.dest_false;
  Format.fprintf fmt " ITEMS (%d)@\n" (Queue.length monkey.items);
  Queue.iter (fun l -> Format.fprintf fmt "  %a@\n" pp_number l) monkey.items

let eval reducer monkeys rounds =
  let get_int div l =
    try List.assoc div l with Not_found -> List.assoc max_int l
  in
  let rec loop r =
    if r < rounds then
      let () =
        Array.iter
          (fun monkey ->
            Queue.iter
              (fun wlevel ->
                let wlevel =
                  List.map
                    (fun (div, v) -> (div, monkey.operation div v))
                    wlevel
                in
                let wlevel =
                  List.rev_map (fun (div, v) -> (div, v / reducer)) wlevel
                in
                let dest =
                  if get_int monkey.divisor wlevel mod monkey.divisor = 0 then
                    monkey.dest_true
                  else monkey.dest_false
                in
                monkey.count <- monkey.count + 1;
                Queue.add wlevel monkeys.(dest).items)
              monkey.items;
            Queue.clear monkey.items)
          monkeys
      in
      loop (r + 1)
  in
  loop 0

let monkey_business monkeys =
  match
    monkeys |> Array.to_list
    |> List.map (fun m -> m.count)
    |> List.sort (fun a b -> compare b a)
  with
  | v1 :: v2 :: _ -> v1 * v2
  | _ -> assert false

let mk_arg = function "old" -> None | s -> Some (int_of_string s)

let mk_operation = function
  | [ arg1_s; op_s; arg2_s ] ->
      let op = List.assoc op_s [ ("+", ( + )); ("*", ( * )) ] in
      let arg1 = mk_arg arg1_s in
      let arg2 = mk_arg arg2_s in
      fun div x ->
        let v1 = match arg1 with None -> x | Some v -> v in
        let v2 = match arg2 with None -> x | Some v -> v in
        op (v1 mod div) (v2 mod div) mod div
  | _ -> failwith "Invalid compute syntax"

let parse_monkeys reducer () =
  let acc = ref [] in
  let divisors = ref [] in
  let rec loop () =
    let _monkey_number = read_line () in
    let items =
      match String.split_on_char ':' (read_line ()) with
      | [ _; lst ] ->
          let values =
            lst |> String.split_on_char ',' |> List.map String.trim
            |> List.map int_of_string
          in
          let q = Queue.create () in
          List.iter (fun x -> Queue.add [ (max_int, x) ] q) values;
          q
      | _ -> failwith "Invalid item spec:"
    in
    let operation =
      match String.split_on_char '=' (read_line ()) with
      | [ _; formula ] ->
          formula |> String.trim |> String.split_on_char ' ' |> mk_operation
      | _ -> failwith "Invalid compute spec"
    in
    let divisor =
      match read_line () |> String.trim |> String.split_on_char ' ' with
      | [ "Test:"; "divisible"; "by"; sn ] -> int_of_string sn
      | _ -> failwith "Invalid criterion"
    in
    let () = divisors := divisor :: !divisors in
    let dest_true =
      match read_line () |> String.trim |> String.split_on_char ' ' with
      | [ "If"; "true:"; "throw"; "to"; "monkey"; sn ] -> int_of_string sn
      | _ -> failwith "Invalid destination true"
    in
    let dest_false =
      match read_line () |> String.trim |> String.split_on_char ' ' with
      | [ "If"; "false:"; "throw"; "to"; "monkey"; sn ] -> int_of_string sn
      | _ -> failwith "Invalid destination false"
    in
    acc :=
      { items; operation; divisor; dest_true; dest_false; count = 0 } :: !acc;
    let _empty = read_line () in
    loop ()
  in
  let monkeys =
    try loop () with End_of_file -> !acc |> List.rev |> Array.of_list
  in
  let () =
    Array.iter
      (fun monkey ->
        let l =
          Queue.fold
            (fun acc -> function
              | [ (_, x) ] as l ->
                  (if reducer <> 1 then l
                  else List.map (fun d -> (d, x)) !divisors)
                  :: acc
              | _ -> acc)
            [] monkey.items
        in
        Queue.clear monkey.items;
        List.iter (fun e -> Queue.add e monkey.items) l)
      monkeys
  in
  monkeys

let solve reducer rounds () =
  let monkeys = parse_monkeys reducer () in
  let () = eval reducer monkeys rounds in
  let mb = monkey_business monkeys in
  Format.printf "%d\n" mb

let name = "11_part1"
let () = Solution.register name (solve 3 20)
let name = "11_part2"
let () = Solution.register name (solve 1 10000)
