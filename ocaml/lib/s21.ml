open Utils.Syntax.Hashtbl

type frac = { num : int; denom : int }

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let sign a = if a >= 0 then 1 else -1

let pp_frac fmt = function
  | { num; denom = 1 } -> Format.fprintf fmt "%d" num
  | { num; denom } -> Format.fprintf fmt "%d/%d" num denom

let frac a b =
  if a = 0 then { num = 0; denom = 1 }
  else
    let sa = sign a in
    let sb = sign b in
    let a = abs a in
    let b = abs b in
    let c = gcd a b in
    { num = sb * sa * a / c; denom = b / c }

let add_frac a b =
  frac ((a.num * b.denom) + (a.denom * b.num)) (a.denom * b.denom)

let sub_frac a b = add_frac a { b with num = -b.num }
let neg_frac a = sub_frac { num = 0; denom = 1 } a
let mul_frac a b = frac (a.num * b.num) (a.denom * b.denom)

let inv_frac a =
  assert (a.num != 0);
  frac a.denom a.num

let div_frac a b = mul_frac a (inv_frac b)
let int_frac num = { num; denom = 1 }

type op = Add | Sub | Mul | Div
type var = string
type expr = Frac of frac | Var of var | Binop of expr * op * expr

let op_of_string = function
  | "+" -> Add
  | "-" -> Sub
  | "*" -> Mul
  | "/" -> Div
  | _ -> assert false

let string_of_op = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let rec pp_expr fmt expr =
  match expr with
  | Frac f -> pp_frac fmt f
  | Var v -> Format.fprintf fmt "%s" v
  | Binop (e1, op, e2) ->
      Format.fprintf fmt "@[(@[%a@])%s(@[%a@])@]" pp_expr e1 (string_of_op op)
        pp_expr e2

let eval_op a op b =
  match op with
  | Add -> add_frac a b
  | Sub -> sub_frac a b
  | Mul -> mul_frac a b
  | Div -> div_frac a b

let inv_op = function Add -> Sub | Sub -> Add | Mul -> Div | Div -> Mul

let read_prog () =
  Utils.fold_fields ' '
    (fun g -> function
      | [ res; n ] ->
          g.%[Filename.chop_suffix res ":"] <- Frac (int_frac @@ int_of_string n);
          g
      | [ res; e1; op; e2 ] ->
          g.%[Filename.chop_suffix res ":"] <-
            Binop (Var e1, op_of_string op, Var e2);
          g
      | _ -> g)
    ~%[]

let eval_prog g =
  let rec eval = function
    | Frac v -> v
    | Var n -> begin
        match g.%[n] with
        | Frac v -> v
        | e ->
            let v = eval e in
            g.%[n] <- Frac v; v
      end
    | Binop (n1, op, n2) ->
        let v1 = eval n1 in
        let v2 = eval n2 in
        eval_op v1 op v2
  in
  eval (Var "root")

let rec eval_equal f e =
  match e with
  | Frac _ -> f, e
  | Var "humn" -> f, e
  | Binop (Frac f1, Add, e2) -> eval_equal (sub_frac f f1) e2
  | Binop (Frac f1, Sub, e2) -> eval_equal (sub_frac f1 f) e2
  | Binop (Frac f1, Mul, e2) -> eval_equal (div_frac f f1) e2
  | Binop (Frac f1, Div, e2) -> eval_equal (div_frac f1 f) e2
  | Binop (e1, Add, (Frac _ as e2)) -> eval_equal f (Binop (e2, Add, e1))
  | Binop (e1, Sub, (Frac _ as e2)) ->
      eval_equal (neg_frac f) (Binop (e2, Sub, e1))
  | Binop (e1, Mul, (Frac _ as e2)) -> eval_equal f (Binop (e2, Mul, e1))
  | Binop (e1, Div, (Frac _ as e2)) ->
      eval_equal (inv_frac f) (Binop (e2, Div, e1))
  | _ -> f, e

let eval_symb g n =
  let rec eval = function
    | Frac v -> Frac v
    | Var "humn" as e -> e
    | Var n -> begin
        match g.%[n] with
        | Frac _ as e -> e
        | e ->
            let v = eval e in
            g.%[n] <- v; v
        | exception Not_found -> Var n
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
    let res = eval_prog (read_prog ()) in
    Format.printf "%a@\n" pp_frac res

  let solve_part2 () =
    let prog = read_prog () in
    let x1, x2 =
      match prog.%["root"] with
      | Binop (Var x1, _, Var x2) -> x1, x2
      | _ -> assert false
    in
    let s1 = eval_symb prog x1 in
    let s2 = eval_symb prog x2 in
    let f, _e =
      match s1, s2 with
      | Frac f1, _ -> eval_equal f1 s2
      | _, Frac f2 -> eval_equal f2 s1
      | _ -> assert false
    in
    Format.printf "%a@\n%!" pp_frac f;
end

let () = Solution.register_mod (module Sol)