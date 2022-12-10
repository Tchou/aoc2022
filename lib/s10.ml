let solve compute get_result () =
  let rec loop current_cycle current_value =
    match String.split_on_char ' ' (read_line ()) with
    | [ "noop" ] ->
        compute current_cycle current_value;
        loop (current_cycle + 1) current_value
    | [ "addx"; sv ] ->
        compute current_cycle current_value;
        compute (current_cycle + 1) current_value;
        let v = int_of_string sv in
        loop (current_cycle + 2) (current_value + v)
    | _ -> assert false
    | exception End_of_file ->
        (* compute current_cycle current_value; *)
        get_result ()
  in
  let res = loop 1 1 in
  Format.printf "%d@\n" res

let compute1, get_result1 =
  let total = ref 0 in
  let target_cycle = ref 20 in
  let c1 cur_cycle cur_value =
    Format.printf "%d -> %d\n" cur_cycle cur_value;
    if cur_cycle = !target_cycle then begin
      total := !total + (!target_cycle * cur_value);
      target_cycle := 40 + !target_cycle
    end
  in
  let g1 () = !total in
  (c1, g1)

let name = "10a"
let () = Solution.register name (solve compute1 get_result1)

let compute2 cur_cycle cur_value =
  let line_pos = (cur_cycle - 1) mod 40 in
  let c =
    if cur_value - 1 <= line_pos && line_pos <= cur_value + 1 then "█" else " "
  in
  if line_pos = 0 then Format.printf "%3d: " cur_cycle;
  Format.printf "%s" c;
  if line_pos = 39 then Format.printf "@\n"

let name = "10b"

let () =
  Solution.register name
    (solve compute2 (fun () ->
         Format.printf "@\n";
         0))
