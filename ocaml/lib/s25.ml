let snafu_digit = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '-' -> -1
  | '=' -> -2
  | _ -> assert false

let digit_snafu = function
  | 0 -> 0, "0"
  | 1 -> 0, "1"
  | 2 -> 0, "2"
  | 3 -> 1, "="
  | 4 -> 1, "-"
  | _ -> assert false

let int_of_snafu s =
  let rec loop acc pow i =
    if i < 0 then acc
    else loop (acc + (pow * snafu_digit s.[i])) (pow * 5) (i - 1)
  in
  loop 0 1 (String.length s - 1)

let snafu_of_int n =
  let rec loop n acc =
    if n = 0 then acc
    else
      let r = n mod 5 in
      let carry, digit = digit_snafu r in
      loop ((n / 5) + carry) (digit :: acc)
  in
  match loop n [] with [] -> "0" | l -> String.concat "" l

let load_numbers () = Utils.fold_lines (fun acc l -> l :: acc) [] |> List.rev

module Sol = struct
  let name = "25"

  let solve_part1 () =
    let l = load_numbers () in
    let sum = List.fold_left (fun acc i -> acc + int_of_snafu i) 0 l in
    let u = snafu_of_int sum in
    Format.eprintf "SUM=%d@\n%!" sum;
    Format.printf "%s@\n%!" u

  let solve_part2 () = assert false
end

let () = Solution.register_mod (module Sol)
