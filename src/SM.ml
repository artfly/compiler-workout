open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let eval_insn (stack, (state, is, os)) insn = match insn with
  | CONST(x) -> (x::stack, (state, is, os))
  | BINOP(op) -> (match stack with
    | x::y::tl -> (((Language.Expr.eval_op op) x y)::tl, (state, is, os))
    | _ -> failwith(Printf.sprintf "Stack doesn't contain enough values"))
  | READ -> (match is with 
    | hd::tl -> (hd::stack, (state, tl, os))
    | _ -> failwith(Printf.sprintf "Input is empty"))
  | WRITE -> (match stack with
    | hd::tl -> (tl, (state, is, os @ [hd]))
    | _ -> failwith(Printf.sprintf "Stack is empty"))
  | LD(x) -> ((state x)::stack, (state, is, os))
  | ST(x) -> (match stack with
    | hd::tl -> (tl, ((Language.Expr.update x hd state), is, os))
    | _ -> failwith(Printf.sprintf "Stack is empty"))

let eval config prg = (List.fold_left eval_insn config) prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
let rec compile_expr e = match e with
  | Language.Expr.Const(c) -> [CONST(c)]
  | Language.Expr.Var(x) -> [LD(x)]
  | Language.Expr.Binop(op, e1, e2) -> (compile_expr e2) @ (compile_expr e1) @ [BINOP(op)]

let rec compile s = match s with
  | Language.Stmt.Read(x) -> [READ; ST(x)]
  | Language.Stmt.Write(e) -> (compile_expr e) @ [WRITE]
  | Language.Stmt.Assign(x, e) -> (compile_expr e) @ [ST(x)]
  | Language.Stmt.Seq(s1, s2) -> compile s1 @compile s2
