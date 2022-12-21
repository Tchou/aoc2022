open Utils.Syntax.Hashtbl

module Frac = struct
  type t = { num : int; denom : int }

  let pp fmt = function
    | { num; denom = 1 } -> Format.fprintf fmt "%d" num
    | { num; denom } -> Format.fprintf fmt "%d/%d" num denom

  let rec gcd a b = if b = 0 then a else gcd b (a mod b)

  (* smart constructor *)
  let mk a b =
    if a = 0 then { num = 0; denom = 1 }
    else
      let va = abs a in
      let vb = abs b in
      let sa = a / va in
      let sb = b / vb in
      let c = gcd a b in
      { num = sb * sa * a / c; denom = b / c }

  let ( +/ ) a b = mk ((a.num * b.denom) + (a.denom * b.num)) (a.denom * b.denom)
  let ( -/ ) a b = a +/ { b with num = -b.num }
  let ( */ ) a b = mk (a.num * b.num) (a.denom * b.denom)
  let ( // ) a b = a */ mk b.denom b.num
  let int num = { num; denom = 1 }
end

type op = Add | Sub | Mul | Div
type var = string
type expr = Frac of Frac.t | Var of var | Binop of expr * op * expr

let op_of_string = function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> assert false

let string_of_op = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
let level = function Add | Sub -> 1 | Mul | Div -> 5

let pp_expr fmt expr =
  let rec loop lvl fmt expr =
    match expr with
    | Frac f -> Frac.pp fmt f
    | Var v -> Format.fprintf fmt "%s" v
    | Binop (e1, op, e2) ->
        let l = level op in
        if lvl > level op then Format.fprintf fmt "(";
        Format.fprintf fmt "@[%a %s %a@]" (loop l) e1 (string_of_op op) (loop l)
          e2;
        if lvl > level op then Format.fprintf fmt ")"
  in
  loop 0 fmt expr

let eval_op a op b =
  let open Frac in
  match op with Add -> a +/ b | Sub -> a -/ b | Mul -> a */ b | Div -> a // b

let inv_op = function Add -> Sub | Sub -> Add | Mul -> Div | Div -> Mul

let read_prog () =
  Utils.fold_fields ' '
    (fun g -> function
      | [ res; n ] ->
          g.%[Filename.chop_suffix res ":"] <- Frac (Frac.int (int_of_string n));
          g
      | [ res; e1; op; e2 ] ->
          g.%[Filename.chop_suffix res ":"] <-
            Binop (Var e1, op_of_string op, Var e2);
          g
      | _ -> g)
    ~%[]

let rec solve f e =
  Format.eprintf "%a = %a@\n" Frac.pp f pp_expr e;
  let open Frac in
  match e with
  | Binop (Frac f1, Add, e2) -> solve (f -/ f1) e2
  | Binop (Frac f1, Sub, e2) -> solve (f1 -/ f) e2
  | Binop (Frac f1, Mul, e2) -> solve (f // f1) e2
  | Binop (Frac f1, Div, e2) -> solve (f1 // f) e2
  | Binop (e1, op, Frac f2) -> solve (eval_op f (inv_op op) f2) e1
  | _ -> f, e

(* evaluate the definition of variable [n] in program [prog]
   If ~sym is given, it is the name of a variable that remains
   symbolic throughout the computation.
   *)
let eval_for_var ?sym prog n =
  let rec eval = function
    | Frac v -> Frac v
    | Var m as e -> begin
        match sym with
        | Some x when m = x -> e
        | _ -> begin
            match prog.%[m] with
            | Frac _ as e -> e
            | e ->
                let v = eval e in
                prog.%[m] <- v; v
            | exception Not_found -> failwith ("No definition for variable " ^ m)
          end
      end
    | Binop (e1, op, e2) -> (
        let v1 = eval e1 in
        let v2 = eval e2 in
        match v1, v2 with
        | Frac f1, Frac f2 -> Frac (eval_op f1 op f2)
        | _ -> Binop (v1, op, v2))
  in

  eval (Var n)

module Sol = struct
  let name = "21"

  let solve_part1 () =
    let res = eval_for_var (read_prog ()) "root" in
    Format.printf "%a@\n" pp_expr res

  let solve_part2 () =
    let prog = read_prog () in
    match prog.%["root"] with
    | Binop (Var x1, _, Var x2) ->
        let s1 = eval_for_var ~sym:"humn" prog x1 in
        let s2 = eval_for_var ~sym:"humn" prog x2 in
        let f, _e =
          match s1, s2 with
          | Frac f, e | e, Frac f -> solve f e
          | _ -> assert false
        in
        Format.printf "%a@\n%!" Frac.pp f
    | _ -> assert false
end

let () = Solution.register_mod (module Sol)