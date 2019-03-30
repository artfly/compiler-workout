(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
    *)                                                       
    let to_int x = if x then 1 else 0
    let cast_int_op op = fun x y -> to_int (op x y)
    let to_bool i = if i = 0 then false else true
    let cast_bool_op op = fun x y -> to_int (op (to_bool x) (to_bool y))

    let eval_op op = match op with
      | "-" -> ( - )
      | "+" -> ( + )
      | "/" -> ( / )
      | "*" -> ( * )
      | "%" -> ( mod ) 
      | "==" -> cast_int_op ( == )
      | "!=" -> cast_int_op ( != )
      | "<=" -> cast_int_op ( <= )
      | "<" -> cast_int_op ( < )
      | ">=" -> cast_int_op ( >= )
      | ">" -> cast_int_op ( > )
      | "&&" -> cast_bool_op ( && ) 
      | "!!" -> cast_bool_op ( || )
      | _ -> failwith(Printf.sprintf "Undefined operator for binary expression")

    let rec eval s e = match e with
      | Const (c) -> c
      | Var (x) -> s x
      | Binop (op, x, y) -> eval_op op (eval s x) (eval s y)

    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    let binop op = ostap(- $(op)), (fun x y -> Binop (op, x, y))

    ostap (
      expr:
        !(Ostap.Util.expr
            (fun x -> x)
            (Array.map (fun (assoc, ops) -> assoc, List.map binop ops)
              [|
                `Lefta , ["!!"];
                `Lefta , ["&&"];
                `Nona , ["<="; ">="; "=="; "!="; ">"; "<";];
                `Lefta , ["+"; "-"];
                `Lefta , ["*"; "/"; "%"];
              |]
            )
            primary
        );

      primary: x:IDENT {Var x} | c:DECIMAL {Const c} | -"(" expr -")"
    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t with show
                                                                    
    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval ((state, is, os): config) (s:t) : config = match s with
      | Read(x) -> (match is with
        | [] -> failwith(Printf.sprintf "No more input")
        | hd::tl -> (Expr.update x hd state, tl, os))
      | Write(e) -> (state, is, os @ [(Expr.eval state e)])
      | Assign(x, e) -> ((Expr.update x (Expr.eval state e) state), is, os)
      | Seq(s1, s2) -> (eval (eval (state, is, os) s1) s2)
      | If(cond, then_branch, else_branch) ->
        let branch = if (Expr.eval state cond) != 0 then then_branch else else_branch in
          eval (state, is, os) branch
      | While(cond, body) ->  if (Expr.eval state cond) != 0
        then let cfg = eval (state, is, os) body in eval cfg s 
        else (state, is, os)
      | Repeat(body, cond) -> let (st, i, o) = eval (state, is, os) body in
        if (Expr.eval st cond) = 0 then eval (st, i, o) s else (st, i, o) 
      | Skip -> (state, is, os)
                        
    (* Statement parser *)
    ostap (
      simple_stmt:
        x:IDENT ":=" e:!(Expr.expr) {Assign(x, e)}
      | "read" "(" x:IDENT ")" {Read x}
      | "write" "(" e:!(Expr.expr) ")" {Write e}
      | "skip" {Skip}
      | "if" cond:!(Expr.expr) "then" then_br:stmts else_br:else_branch {If(cond, then_br, else_br)}
      | "while" cond:!(Expr.expr) "do" body:loop_body {While(cond, body)}
      | "repeat" body:repeat_body cond:!(Expr.expr) {Repeat(body, cond)}
      | "for" init:simple_stmt "," cond:!(Expr.expr) "," update:simple_stmt "do" body:loop_body {Seq(init, While(cond, Seq(body, update)))}
      ;

      repeat_body: ss:simple_stmt ";" rest:repeat_body {Seq(ss, rest)} | ss:simple_stmt "until" {ss};

      loop_body: ss:simple_stmt ";" rest:loop_body {Seq(ss, rest)} | ss:simple_stmt "od" {ss};

      else_branch: "elif" cond:!(Expr.expr) "then" ss:simple_stmt rest:else_branch {If(cond, ss, rest)} | "else" s:stmts "fi" {s} | "fi" {Skip};
      
      stmts: ss:simple_stmt ";" rest:stmts {Seq(ss, rest)} | ss:simple_stmt {ss};

      parse: ss:simple_stmt ";" rest:parse {Seq(ss, rest)} | simple_stmt
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
