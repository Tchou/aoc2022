open Utils.Syntax.Hashtbl

module Resource = struct
  type t = int * int * int (*ore * clay * obsidian *)

  let ( +: ) (a, b, c) (d, e, f) = a + d, b + e, c + f
  let ( -: ) (a, b, c) (d, e, f) = a - d, b - e, c - f
  let ( *: ) d (a, b, c) = a * d, b * d, c * d
  let ( >=: ) (a, b, c) (d, e, f) = a >= d && b >= e && c >= f
  let zero = 0, 0, 0
  let ore = 1, 0, 0
  let clay = 0, 1, 0
  let obsidian = 0, 0, 1
  let get_ore (a, _, _) = a
  let get_clay (_, b, _) = b
  let get_obs (_, _, c) = c
  let pp fmt (a, b, c) = Format.fprintf fmt "ore:%d, clay: %d, obs:%d" a b c
end

type config = {
  resource : Resource.t;
  ore_robot : int;
  clay_robot : int;
  obsidian_robot : int;
  geode_robot : int;
}

let default_config =
  {
    resource = Resource.zero;
    ore_robot = 1;
    clay_robot = 0;
    obsidian_robot = 0;
    geode_robot = 0;
  }

type blueprint = {
  ore_robot_cost : Resource.t;
  clay_robot_cost : Resource.t;
  obsidian_robot_cost : Resource.t;
  geode_robot_cost : Resource.t;
}

let pp_config fmt c =
  Format.fprintf fmt "{%a, ore_r: %d, cl_r: %d, ob_r:%d, ge_r: %d}" Resource.pp
    c.resource c.clay_robot c.obsidian_robot c.geode_robot

(* For each robot 3 functions :
    - do we need the robot : we need robots until we have enough robots to
      cover the production of 1 other robot (since we can only buy robot a day)
    - can we pay for the robot
    - increment the number of robots by 1
*)
(* ore robots *)
let need_ore_robot bp c =
  c.ore_robot
  < Resource.get_ore
      (max bp.ore_robot_cost
         (max bp.clay_robot_cost
            (max bp.obsidian_robot_cost bp.geode_robot_cost)))

let has_robot_for cost c =
  let open Resource in
  (get_ore cost = 0 || c.ore_robot > 0)
  && (get_clay cost = 0 || c.clay_robot > 0)
  && (get_obs cost = 0 || c.obsidian_robot > 0)

let div_sup a b = (a / b) + if a mod b = 0 then 0 else 1

let pay_robot cost c =
  let open Resource in
  if c.resource >=: cost then Some (1, { c with resource = c.resource -: cost })
  else if has_robot_for cost c then
    let ore_c = get_ore cost in
    let clay_c = get_clay cost in
    let obs_c = get_obs cost in
    let ore_c =
      if ore_c = 0 then 0 else div_sup (ore_c - get_ore c.resource) c.ore_robot
    in
    let clay_c =
      if clay_c = 0 then 0
      else div_sup (clay_c - get_clay c.resource) c.clay_robot
    in
    let obs_c =
      if obs_c = 0 then 0
      else div_sup (obs_c - get_obs c.resource) c.obsidian_robot
    in
    let d = max ore_c (max clay_c obs_c) + 1 in
    Some (d, { c with resource = c.resource -: cost })
  else None

let incr_ore_robot c = { c with ore_robot = c.ore_robot + 1 }

(* clay robots *)
let need_clay_robot bp c =
  c.clay_robot < Resource.get_clay bp.obsidian_robot_cost

let incr_clay_robot c = { c with clay_robot = c.clay_robot + 1 }

(* obsidian robot *)
let need_obsidian_robot bp c =
  c.obsidian_robot < Resource.get_obs bp.geode_robot_cost

let incr_obsidian_robot c = { c with obsidian_robot = c.obsidian_robot + 1 }

(* geode robot *)
let need_geode_robot _ _ = true (* we need them !*)
let incr_geode_robot c = { c with geode_robot = c.geode_robot + 1 }

(* compute (since I'm lazy) the number of geode we can produce in d days
     at full capacity knowing that one geode robot is produced each day *)
let max_geode_prudction c d =
  let d1 = d + 1 in
  (c.geode_robot * d1) + (d * d1 / 2)

let operations bp =
  [|
    need_geode_robot, pay_robot bp.geode_robot_cost, incr_geode_robot;
    need_obsidian_robot, pay_robot bp.obsidian_robot_cost, incr_obsidian_robot;
    need_clay_robot, pay_robot bp.clay_robot_cost, incr_clay_robot;
    need_ore_robot, pay_robot bp.ore_robot_cost, incr_ore_robot;
  |]

(* update the production *)
let update d c =
  let open Resource in
  {
    c with
    resource =
      c.resource
      +: (d * c.ore_robot *: ore)
      +: (d * c.clay_robot *: clay)
      +: (d * c.obsidian_robot *: obsidian);
  }

let simulate n blueprint start =
  let cache = ~%[] in
  let max_geode = ref 0 in
  let operations = operations blueprint in
  let rec loop i config geode =
    if i > n then begin
      max_geode := max !max_geode geode;
      geode
    end
    else
      match cache.%[i, config] with
      | r -> r + geode
      | exception Not_found ->
          let res =
            if max_geode_prudction config (n - i) + geode < !max_geode then
              geode
            else
              Array.fold_left
                (fun acc (need, pay, incr) ->
                  if need blueprint config then
                    match pay config with
                    | Some (d, nconfig) ->
                        let d = min d (n - i + 1) in
                        let nconfig = update d nconfig in
                        (* update production *)
                        let geode = geode + (d * nconfig.geode_robot) in
                        let nconfig = incr nconfig in
                        (* obtain the robot after update *)
                        max acc (loop (i + d) nconfig geode)
                    | None -> acc
                  else acc)
                geode operations
          in
          cache.%[i, config] <- res - geode;
          res
  in
  loop 1 start 0

let load_blueprints () =
  let open Resource in
  Utils.fold_lines
    (fun acc s ->
      Scanf.sscanf s
        "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d \
         ore. Each obsidian robot costs %d ore and %d clay. Each geode robot \
         costs %d ore and %d obsidian." (fun i oo co obo obcl go gob ->
          ( i,
            {
              ore_robot_cost = oo *: ore;
              clay_robot_cost = co *: ore;
              obsidian_robot_cost = (obo *: ore) +: (obcl *: clay);
              geode_robot_cost = (go *: ore) +: (gob *: obsidian);
            } )
          :: acc))
    []
  |> List.rev

let run_blueprints f init bps n =
  let res =
    List.fold_left
      (fun acc (i, bp) ->
        Format.eprintf "Running blueprint %d@\n%!" i;
        let t0 = Unix.gettimeofday () in
        let geode = simulate n bp default_config in
        let t1 = Unix.gettimeofday () in
        Format.eprintf "Found %d geodes in %fms@\n%!" geode (1000. *. (t1 -. t0));
        f acc i geode)
      init bps
  in
  Format.printf "%d@\n" res

module Sol = struct
  let name = "19"

  let solve_part1 () =
    let blueprints = load_blueprints () in
    run_blueprints (fun acc i geode -> acc + (i * geode)) 0 blueprints 24

  let solve_part2 () =
    let blueprints =
      match load_blueprints () with
      | ([] | [ _ ] | [ _; _ ]) as b -> b
      | x :: y :: z :: _ -> [ x; y; z ]
    in
    run_blueprints (fun acc _ geode -> acc * geode) 1 blueprints 32
end

let () = Solution.register_mod (module Sol)
