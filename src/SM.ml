open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP   of string
(* put a constant on the stack     *) | CONST   of int
(* put a string on the stack       *) | STRING  of string                      
(* load a variable to the stack    *) | LD      of string
(* store a variable from the stack *) | ST      of string
(* store in an array               *) | STA     of string * int
(* a label                         *) | LABEL   of string
(* unconditional jump              *) | JMP     of string
(* conditional jump                *) | CJMP    of string * string
(* begins procedure definition     *) | BEGIN   of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL    of string * int * bool
(* returns from a function         *) | RET     of bool with show
                                                   
(* The type for the stack machine program *)
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
*)
type config = (prg * State.t) list * Value.t list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)
let split n l =
  let rec unzip (taken, rest) = function
  | 0 -> (List.rev taken, rest)
  | n -> let h::tl = rest in unzip (h::taken, tl) (n-1)
  in
  unzip ([], l) n
        
let to_str insn = match insn with
  | CONST(x) -> "CONST " ^ string_of_int x
  | BINOP(op) -> op
  | LD(x) -> "LD " ^ x
  | ST(x) -> "ST " ^ x
  | JMP(l) -> "JMP " ^ l
  | CJMP(cnd, l) -> "CJMP_" ^ cnd ^ " " ^ l
  | LABEL(l) -> "LABEL " ^ l
  | CALL(f, _, _) -> "CALL " ^ f
  | BEGIN(_, _, _) -> "BEGIN"
  | END -> "END"
  | RET _ -> "RET"

let rec print_prg prg = 
  let prg' = List.map (fun insn -> to_str insn) prg in
  let txt = List.fold_left (fun acc insn -> acc ^ "\n" ^ insn) "" prg' in 
  Printf.printf "%s" txt; prg

let rec eval env ((cstack, stack, ((st, i, o) as c)) as conf) prg = match prg with
  | insn::rest -> (match insn with
    | CONST(x)                 -> let conf = (cstack, (Value.of_int x)::stack, c) in eval env conf rest
    | STRING(s)                -> let conf = cstack, (Value.of_string s)::stack, c in eval env conf rest
    | BINOP(op)                -> (match stack with y::x::stack' -> let v = ((Expr.to_func op) (Value.to_int x) (Value.to_int y)) in 
                                                                    let conf' = cstack, (Value.of_int v)::stack', c in
                                                                    eval env conf' rest)
    | LD(x)                    -> let v = (State.eval st x) in let conf' = cstack, v::stack, c in eval env conf' rest
    | ST(x)                    -> (match stack with v::stack' -> let st' = State.update x v st in 
                                                                 let conf' = cstack, stack', (st', i, o) in 
                                                                 eval env conf' rest)
    | STA(x, n)                -> let (v::vs, stack') = split (n+1) stack in
                                  let st' = (Stmt.update st x v vs) in
                                  let conf' = cstack, stack', (st', i, o) in
                                  eval env conf' rest
    | JMP(l)                   -> let prg' = env#labeled l in eval env conf prg'
    | CJMP(cnd, l)             -> (match stack with v::stack' -> let prg' = if ((cnd = "z") = ((Value.to_int v) = 0)) then env#labeled l else rest in 
                                                                 let conf = cstack, stack', c in
                                                                 eval env conf prg')
    | LABEL(_)                 -> eval env conf rest
    | CALL(f, n, p)            -> if env#is_label f then
                                    let conf' = (rest, st)::cstack, stack, c in
                                    eval env conf' (env#labeled f)
                                  else
                                    let conf' = env#builtin conf f n p in
                                    eval env conf' rest
    | BEGIN(_, params, locals) -> let (st', stack') = Stmt.enter params stack locals st in 
                                  let conf' = cstack, stack', (st', i, o) in
                                  eval env conf' rest
    | END | RET _              -> (match cstack with
                                  | (prg', st')::cstack' -> let st'' = State.leave st st' in
                                                            let conf' = cstack', stack, (st'', i, o) in
                                                            eval env conf' prg'
                                  | [] -> conf)
    )
  | [] -> conf

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
  let (_, _, (_, _, o)) =
    eval
      (object
         method is_label l = M.mem l m
         method labeled l = M.find l m
         method builtin (cstack, stack, (st, i, o)) f n p =
           let f = match f.[0] with 'L' -> String.sub f 1 (String.length f - 1) | _ -> f in
           let args, stack' = split n stack in
           let (st, i, o, r) = Language.Builtin.eval (st, i, o, None) (List.rev args) f in
           let stack'' = if p then stack' else let Some r = r in r::stack' in
           Printf.printf "Builtin: %s\n";
           (cstack, stack'', (st, i, o))
       end
      )
      ([], [], (State.empty, i, []))
      p
  in
  o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
class labels = 
  object(self)
    val mutable cnt = 0
    method make = let l = ".L" ^ string_of_int cnt in cnt <- cnt + 1; l
  end

let rec compile (defs, stmt) = let labels = (new labels)
  in
    let rec compile_expr e = match e with
      | Expr.Const(c)          -> [CONST(c)]
      | Expr.String(s)         -> [STRING(s)]
      | Expr.Var(x)            -> [LD(x)]
      | Expr.Length(e)         -> compile_expr e @ [CALL("$length", 1, false)]
      | Expr.Array(xs)         -> compile_list (List.rev xs) @ [CALL("$array", List.length xs, false)]
      | Expr.Elem(a, is)       -> compile_list [is; a] @ [CALL("$elem", 2, false)]
      | Expr.Binop(op, e1, e2) -> compile_expr e1 @ compile_expr e2 @ [BINOP(op)]
      | Expr.Call(f, args)     -> (List.fold_left (fun acc arg -> (compile_expr arg) @ acc) [] args) @ [CALL(f, List.length args, false)]
    and 
    compile_list xs = (List.fold_left (fun acc x -> (compile_expr x) @ acc) [] xs)
  in
    let rec compile_if (c, s1, s2) fi = 
      let rec compile_then (s: Stmt.t) = match s with
        | Seq(s1, s2)   -> compile_stmt s1 @ compile_then s2
        | If(c, s1, s2) -> compile_if (c, s1, s2) fi
        | _             -> compile_stmt s
      in
      let else_lbl = labels#make in
      compile_expr c @ [CJMP("z", else_lbl)] @ 
      compile_then s1 @ [JMP(fi)] @
      [LABEL(else_lbl)] @ compile_stmt s2

    and compile_stmt stmt = match stmt with
      | Assign(x, is, e) -> let n = List.length is in (List.fold_left (fun acc arg -> (compile_expr arg) @ acc) [] is) @ compile_expr e @ [if n = 0 then ST(x) else STA(x, n)]
      | Seq(s1, s2)   -> compile_stmt s1 @ compile_stmt s2
      | If(c, s1, s2) -> let fi = labels#make in compile_if (c, s1, s2) fi @ [LABEL(fi)]
      | While(c, s)   -> let cond = labels#make in let loop = labels#make in
                         [JMP(cond)] @
                         [LABEL(loop)] @ compile_stmt s @
                         [LABEL(cond)] @ compile_expr c @
                         [CJMP("nz", loop)]
      | Repeat(s, c)  -> let loop = labels#make in
                         [LABEL(loop)] @ compile_stmt s @
                         compile_expr c @ [CJMP("z", loop)]
      | Skip          -> []
      | Return(e)     -> (match e with Some(e) -> compile_expr e | None -> []) @ [RET(e <> None)]
      | Call(f, args) -> (List.fold_left (fun acc arg -> (compile_expr arg) @ acc) [] args) @ [CALL(f, List.length args, true)]
  in
    let compile_def (name, (params, locals, s)) = [LABEL(name); BEGIN(name, params, locals)] @ (compile_stmt s) @ [END]
  in
    compile_stmt stmt @ [END] @ 
    List.fold_left (fun acc def -> acc @ compile_def def) [] defs
