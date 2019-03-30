open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string                                                                                                                
(* conditional jump                *) | CJMP  of string * string with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                         

let eval_insn (stack, (state, is, os)) insn = match insn with
  | CONST(x) -> (x::stack, (state, is, os))
  | BINOP(op) ->  let x::y::stack' = stack in ((Language.Expr.eval_op op) y x)::stack', (state, is, os)
  | READ -> let x::is' = is in x::stack, (state, is', os)
  | WRITE -> let x::stack' = stack in stack', (state, is, os @ [x])
  | LD(x) -> (state x)::stack, (state, is, os)
  | ST(x) -> let v::stack' = stack in let state' = (Language.Expr.update x v state) in (stack', (state', is, os))
  

let rec eval env cfg prg = let (stack, (state, is, os)) = cfg in match prg with
  | [] -> cfg
  | insn::rest -> match insn with
    | JMP(l) -> (eval env cfg (env#labeled l))
    | CJMP(cond, l) -> 
      let x::stack' = stack in 
      let jmp = if cond = "z" then x != 0 else x = 0 in
        if jmp then (eval env cfg (env#labeled l)) else (eval env cfg rest)
    | LABEL(_) -> (eval env cfg rest)
    | _ -> let cfg' = (eval_insn cfg insn) in (eval env cfg' rest)

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], (Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class labels = 
  object(self)
    val mutable cnt = 0
    method make = let l = ".L" ^ (string_of_int cnt) in cnt <- cnt + 1; l
  end

let rec compile_expr e = match e with
  | Language.Expr.Const(c) -> [CONST(c)]
  | Language.Expr.Var(x) -> [LD(x)]
  | Language.Expr.Binop(op, e1, e2) -> (compile_expr e1) @ (compile_expr e2) @ [BINOP(op)]

let rec compile_then s fi_label labels = match s with
  | Language.Stmt.Seq(s1, s2) -> (compile' s1 labels) @ (compile_then s2 fi_label labels)
  | Language.Stmt.If(cond, then_branch, else_branch) -> (compile_if (cond, then_branch, else_branch) fi_label labels)
  | _ -> (compile' s labels)

and compile_if (cond, then_branch, else_branch) fi_label labels =
  let op = Language.Expr.Binop("==", Const(0), cond) in 
  let else_label = labels#make in
    (compile_expr op) @ [CJMP("z", else_label)] @ 
    (compile_then then_branch fi_label labels) @ [JMP(fi_label)] @
    [LABEL(else_label)] @ (compile' else_branch labels)

and compile' s labels = match s with
  | Language.Stmt.Read(x) -> [READ; ST(x)]
  | Language.Stmt.Write(e) -> (compile_expr e) @ [WRITE]
  | Language.Stmt.Assign(x, e) -> (compile_expr e) @ [ST(x)]
  | Language.Stmt.Seq(s1, s2) -> (compile' s1 labels) @ (compile' s2 labels)
  | Language.Stmt.If(cond, then_branch, else_branch) -> 
    let fi_label = labels#make in 
      (compile_if (cond, then_branch, else_branch) fi_label labels) @ [LABEL(fi_label)]
  | Language.Stmt.While(cond, body) -> 
    let check_label = labels#make in
    let loop_label = labels#make in 
    let op = Language.Expr.Binop("==", Const(0), cond) in 
      [JMP(check_label)] @ [LABEL(loop_label)] @ (compile' body labels) @
      [LABEL(check_label)] @ (compile_expr op) @ [CJMP("nz", loop_label)]
  | Language.Stmt.Repeat(body, cond) ->
    let loop_label = labels#make in 
    let op = Language.Expr.Binop("==", Const(0), cond) in
      [LABEL(loop_label)] @ (compile' body labels) @ (compile_expr op) @ [CJMP("z", loop_label)]
  | Language.Stmt.Skip -> []

let rec compile s = compile' s (new labels)