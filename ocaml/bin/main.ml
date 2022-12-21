open Aoc2022

let () =
  let open Format in
  pp_set_margin err_formatter 2000;
  pp_set_max_indent err_formatter 2000;
  pp_set_margin std_formatter 2000;
  pp_set_max_indent std_formatter 2000

let main () =
  if Array.length Sys.argv < 2 then begin
    Format.printf "Usage: %s <problem>@\n" Sys.argv.(0);
    exit 1
  end;
  let arg = Sys.argv.(1) in
  if arg = "list" then begin
    Format.printf "Available solutions:@\n";
    List.iter (Format.printf "    %s@\n") (Solution.list ());
    exit 0
  end;
  match Solution.get Sys.argv.(1) with
  | None ->
      Format.printf "Error: unknown solution %s@\n" arg;
      exit 2
  | Some f -> f ()

let () = main ()