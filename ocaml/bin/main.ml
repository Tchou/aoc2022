let default =
  Aoc2022.Solution.register "000" (fun () -> ());
  "000"

let main () =
  let solver_name =
    if Array.length Sys.argv < 2 then default
    else if Aoc2022.Solution.has Sys.argv.(1) then Sys.argv.(1)
    else default
  in
  Format.eprintf "%s\n%!" solver_name;
  let solve = Aoc2022.Solution.get solver_name in
  solve ()

let () = main ()