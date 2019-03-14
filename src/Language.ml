(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

open List

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
open Ostap
       
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
    let rec eval curState expr =

      let applyOp op =
        let (&) f g x y   = f (g x y) in
        let boolToInt   b = if b then 1 else 0 in
        let boolFromInt i = if abs i > 0 then true else false in
        let boolF f x y   = f (boolFromInt x) (boolFromInt y) in

        match op with
          | "+"  -> (  +  )
          | "-"  -> (  -  )
          | "*"  -> (  *  )
          | "/"  -> (  /  )
          | "%"  -> ( mod )
          | "<"  -> boolToInt & ( < )
          | "<=" -> boolToInt & ( <= )
          | ">"  -> boolToInt & ( >  )
          | ">=" -> boolToInt & ( >= )
          | "==" -> boolToInt & ( == )
          | "!=" -> boolToInt & ( != )
          | "&&" -> boolToInt & boolF ( && )
          | "!!" -> boolToInt & boolF ( || )
          | _    -> failwith "Unknown operation" in

      match expr with
        | Const i                 -> i
        | Var   v                 -> curState v
        | Binop (op, left, right) -> applyOp op (eval curState left) (eval curState right)

      end

    (* Expression parser. You can use the following terminals:
         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    let parseBinop op = ostap(- $(op)), (fun x y -> Expr.Binop (op, x, y))
    ostap (
      expr:
        !(Util.expr
           (fun x -> x)
           (Array.map (fun (a, ops) -> a, List.map parseBinop ops)
                [|
                  `Lefta, ["!!"];
                  `Lefta, ["&&"];
                  `Nona , ["=="; "!="; "<="; ">="; "<"; ">"];
                  `Lefta, ["+"; "-"];
                  `Lefta, ["*"; "/"; "%"];
                |]
           )
           primary
         );

      primary: x:IDENT { Expr.Var x } | n:DECIMAL { Expr.Const n } | -"(" expr -")"
    )

                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator
         val eval : config -> t -> config
       Takes a configuration and a statement, and returns another configuration
    *)
    let rec eval (curState, i, o) t =
      match t with
        | Read    var           -> (Expr.update var (hd i) curState, tl i, o)
        | Write   expr          -> let res = Expr.eval curState expr
                                   in (curState, i, o @ [res])
        | Assign (var, expr)    -> let res = Expr.eval curState expr
                                   in (Expr.update var res curState, i, o)
        | Seq    (expr1, expr2) -> let first = eval (curState, i, o) expr1
                                   in eval first expr2

    (* Statement parser *)
    ostap (
      simpleStmt:
        x:IDENT ":=" e:expr     { Assign (x, e) }
      | "read"  "(" x:IDENT ")" { Read   x      }
      | "write" "(" e:expr  ")" { Write  e      };

      parse: s1:simpleStmt ";" s2:parse { Seq (s1, s2) } | simpleStmt
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