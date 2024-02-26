type op = Add | Mult


type depython =
 | BinOp of depython * op * depython
 | Const of int
 | Name of string
 | Assign of string * depython
 | If of depython * depython * depython

(* sample expressions in depython *)
let t1 = BinOp (Const 5, Add, BinOp (Const 25, Mult, Const 2))

let t2 = [ Assign ("n", Const 2) ; BinOp (Const 5, Add, BinOp (Const 25, Mult, Name "n")) ]

(* environments map variable names like "x" to an int like 3 *)
type env = (string * int) list

(* extend the environment env with a binding of n to name v *)
let guardar ((v : string), (n : int), (env : env)) : (int * env) =
  (n, (v, n) :: env)

(* find the int bound to v in the environment (or fail) *)
let rec lookup (v, env) =
  match env with
  | [] -> failwith "No existe"
  | (x, n)::xs -> if x = v then n else lookup (v, xs)

let rec calc ((e : depython), (env : env)) : (int * env) =
(* given an expression e and an environment env, return the value of e and a
 new environment *)
  match e with
  | Const n -> (n, env)
  | BinOp (l, Add, r) ->
     let lv, env' = calc (l, env) in
     let rv, env'' = calc (r, env') in
     lv + rv, env''
  | BinOp (l, Mult, r) ->
     let lv, env' = calc (l, env) in
     let rv, env'' = calc (r, env') in
     lv * rv, env''
  | Assign (x, e) ->
     let lv, env' = calc (e, env) in
     guardar (x, lv, env')
  | Name x -> (lookup (x, env), env)
  | If (test, then_case, else_case) ->
     let tst, env' = calc (test, env) in
     if (tst != 0) then
       calc (then_case, env)
     else
       calc (else_case, env)


let interp (es : depython list) : int =
  let rec help (es, env, n) =
    match es with
    | [] -> n
    | (e :: es) ->
       let n, env' = calc (e, env) in
       help (es, env', n)
  in
  help (es, [], 0)

let unif = [If (BinOp (Const 1, Add, Const 2), Const 4, Const 5)];;