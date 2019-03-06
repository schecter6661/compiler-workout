open GT       
open Language
open Stmt
       
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

(* Function for failing on not enough elements *)
let lenght_fail str = failwith ("Not enough elements on " ^ str)

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval cfg progs = List.fold_left eval_instr cfg progs
    and eval_instr st_cfg instr =
    let (stack, cfg) = st_cfg in
    let (s, i, o)    = cfg in
    match instr with
        | BINOP op -> (match stack with
            | x::y::stack_rest -> ((Expr.eval_op op y x)::stack_rest, cfg)
            | _                -> lenght_fail "stack")
        | CONST z -> (z::stack, cfg)
        | READ    -> (match i with
            | z::i_rest        -> (z::stack, (s, i_rest, o))
            | _                -> lenght_fail "input")
        | WRITE -> (match stack with
            | z::stack_rest    -> (stack_rest, (s, i, o @ [z]))
            | _                -> lenght_fail "stack")
        | LD x -> ((s x)::stack, cfg)
        | ST x -> (match stack with
            | z::stack_rest    -> (stack_rest, (Expr.update x z s, i, o))
            | _                -> lenght_fail "stack")

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
let rec compile stmt = match stmt with
    | Read    x       -> [READ; ST x]
    | Write   e       -> (compile_expr e)@[WRITE]
    | Assign (x, e)   -> (compile_expr e)@[ST x]
    | Seq    (s1, s2) -> (compile s1)@(compile s2)
    and compile_expr e = match e with
        | Expr.Const  n         -> [CONST n]
        | Expr.Var    x         -> [LD x]
        | Expr.Binop (op, a, b) -> (compile_expr a)@(compile_expr b)@[BINOP op]