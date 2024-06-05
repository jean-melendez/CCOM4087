open Printf
open Ast

type reg = RAX | RSP

type arg =
  | Reg of reg
  | Const of int
  | RegOffset of reg * int

type instruction =
  | Mov of arg * arg
  | Add of arg * arg
  | Mul of arg * arg
  | Cmp of arg * arg
  | Jnz of string
  | Jmp of string
  | Label of string

type env = (string * int) list

let guardar ((v : string), (env : env)) : (int * env) =
  let n = 1 + List.length(env) in
  (n, (v, n) :: env)

let rec lookup (v, env) =
  match env with
  | [] -> failwith "No existe"
  | (x, n)::xs -> if x = v then n else lookup (v, xs)

type tag = int

let rec anf (e : tag ast) : tag aexpr =
  match e with
    | Num (n, tag) -> AImm (ImmNum n, tag)
    | Inc (e, tag) ->
       let tmpvar = sprintf "_tmp_%d" tag in
       ALet (tmpvar, (anf e), AAdd1 (ImmId tmpvar, tag), tag)
    | Dec (e, tag) ->
       let tmpvar = sprintf "_tmp_%d" tag in
       ALet (tmpvar, (anf e), ASub1 (ImmId tmpvar, tag), tag)
    | Let (s, e1, e2, tag) ->
       ALet (s, anf e1, anf e2, tag)
    | Id (s, tag) ->
       AImm (ImmId s, tag)
    | If (c, thn, els, tag) ->
       let tmpvar = sprintf "_if_%d" tag in
       ALet (tmpvar, anf c,
             AIf (ImmId tmpvar, anf thn, anf els, tag), tag)
    | BinOp (e1, op, e2, tag) ->
       let tmpvar1 = sprintf "_bin_l_%d" tag in
       let tmpvar2 = sprintf "_bin_r_%d" tag in
       ALet (tmpvar1, anf e1,
             ALet (tmpvar2, anf e2,
                   APrim2 (op, ImmId tmpvar1, ImmId tmpvar2, tag), tag), tag)

let rec compile_expr (e : tag aexpr) (env : env) : instruction list =
  let imm_to_arg (e : immexpr) =
    match e with
      | ImmNum n ->
         Const n
      | ImmId v ->
         let slot = lookup (v, env) in
         RegOffset(RSP, ~-8*slot)
  in
  match e with
  | AImm (imm, _) ->
     [Mov (Reg RAX, imm_to_arg imm) ]
  | AAdd1 (inc1, _) ->
     [ Mov (Reg RAX, imm_to_arg inc1) ;
       Add (Reg RAX, Const 1) ]
  | ASub1 (dec1, _) ->
     [ Mov (Reg RAX, imm_to_arg dec1) ;
       Add (Reg RAX, Const ~-1) ]
  | ALet (let1, valor, valor2, _) ->
     let (slot, env') = guardar (let1, env) in
     compile_expr valor env @
       [ Mov (RegOffset (RSP, ~-8 * slot), Reg RAX) ] @
         compile_expr valor2 env'
  | AIf (e1, e2, e3, tag) ->
     let lt = sprintf "if_true_%d" tag in
     let lf = sprintf "if_false_%d" tag in
     let ld = sprintf "done_%d" tag in
     [ Mov (Reg RAX, imm_to_arg e1) ;
       Cmp (Reg RAX, Const 0) ;
       Jnz lt ;
       Label lf ] @
       compile_expr e3 env
       @ [ Jmp ld ;
           Label lt ]
       @ compile_expr e2 env
       @ [ Label ld ]
  | APrim2 (Add, imm1, imm2, _) ->
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Add (Reg RAX, imm_to_arg imm2) ]
  | APrim2 (Mul, imm1, imm2, _) ->
     [ Mov (Reg RAX, imm_to_arg imm1) ;
       Mul (Reg RAX, imm_to_arg imm2) ]



let rec asm_to_string instrs =
  match instrs with
  | [] -> ""
  | (i::instrs) ->  (inst_to_string i) ^ (asm_to_string instrs)

and inst_to_string inst =
  match inst with
    | Mov (a1, a2) -> "mov " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Add (a1, a2) -> "add " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Mul (a1, a2) -> "imul " ^ (arg_to_string a1) ^ ", " ^ (arg_to_string a2) ^ "\n"
    | Jnz label -> "jnz " ^ label ^ "\n"
    | Jmp label -> "jmp " ^ label ^ "\n"
    | Label label -> label ^ ":\n"
    | Cmp (a1, a2) -> "cmp " ^ arg_to_string a1 ^ ", " ^ arg_to_string a2 ^ "\n"

and arg_to_string a =
  match a with
  | Reg r -> reg_to_string r
  | Const c -> string_of_int c
  | RegOffset (r, slot) -> "[" ^ reg_to_string r ^ string_of_int slot ^ "]"

and reg_to_string r =
  match r with
  | RAX -> "RAX"
  | RSP -> "RSP"

let optimize (instrs : instruction list) : instruction list =
  (* hagan algo aqui*)
  let rec help buenas quizas =
     match quizas with
     | [] -> List.rev buenas
     | Mov (arg1, arg2)::Mov (arg3, arg4)::mas ->
         if arg1 = arg4 && arg2 = arg3
         then
             help (Mov (arg1, arg2)::buenas) mas
         else if arg1 = arg3 && arg2 = arg4
         then
             help (Mov (arg1, arg2)::buenas) mas
         else
             help (Mov (arg3, arg4)::Mov (arg1, arg2)::buenas) mas
     | i::is -> help (i::buenas) is
  in help [] instrs
  (*instrs*)

let compile_program (program : tag ast) : string =
  let anfed = anf program in
  let instrs = compile_expr anfed [] in
  let optimized = optimize instrs in
  let asm_string = asm_to_string optimized in

  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
  %s
  ret\n" asm_string;;

let tag (e : 'a ast) : tag ast =
  let rec help (e : 'a ast) (cur : tag) : (tag ast * tag) =
    match e with
    | Num (n, _) -> (Num (n, cur), cur + 1)
    | Inc (e, _) ->
      let (tag_e, next_tag) = help e (cur + 1) in
      (Inc (tag_e, cur), next_tag)
    | Dec (e, _) ->
      let (tag_e, next_tag) = help e (cur + 1) in
      (Dec (tag_e, cur), next_tag)
    | Let (v, e1, e2, _) ->
      let (tag_e1, next_tag) = help e1 (cur + 1) in
      let (tag_e2, next_tag) = help e2 (next_tag + 1) in
      (Let (v, tag_e1, tag_e2, cur), next_tag)
    | Id (v, _) -> (Id (v, cur), cur + 1)
    | If (c, t, e, _) ->
      let (tag_c, next_tag) = help c (cur + 1) in
      let (tag_t, next_tag) = help t (next_tag + 1) in
      let (tag_e, next_tag) = help e (next_tag + 1) in
      (If (tag_c, tag_t, tag_e, cur), next_tag)
    | BinOp (e1, op, e2, _) ->
      let (tag_e1, next_tag) = help e1 (cur + 1) in
      let (tag_e2, next_tag) = help e2 (next_tag + 1) in
      (BinOp (tag_e1, op, tag_e2, cur), next_tag)
  in
  let (tagged, _) = help e 1 in tagged;;

(* Some OCaml boilerplate for reading files and command-line arguments *)
(* Use code from https://mukulrathi.com/create-your-own-programming-language/parsing-ocamllex-menhir/ to catch syntax errors *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let maybe_program = Front.parse_file input_file in
  close_in input_file;
  match maybe_program with
  | Ok input_program ->
     let tagged = tag input_program in
     let program = (compile_program tagged) in
     printf "%s\n" program
  | Error e -> eprintf "%s" (Core.Error.to_string_hum e) ; exit 1