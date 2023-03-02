(* AST definition *)
type expr =
  | Int of int
  | Add of expr * expr

let rec eval_as_num : expr -> int  = function 
   | Int a -> a
   | Add (x,y) -> (eval_as_num x) + (eval_as_num y)

let rec eval_as_string : expr -> string = function
   | Int a -> string_of_int a
   | Add (x,y) ->  "(" ^ (eval_as_string x) ^ (eval_as_string y) ^ ")"

let my_ast = Add(Add(Int 1, Int 2), Int 3)
let result_num = eval_as_num my_ast
let result_str = eval_as_string my_ast

(* Finally tagless definition*)
module type Sym = sig
  type 'a repr 
  val int : int -> int repr
  val add : int repr -> int repr -> int repr
  val mult : int repr -> int repr -> int repr (* Adding multiplication doesn't raise an error when pattern-matching is not exhaustive*)
end

module InterpretAsNum = struct
  type 'a repr = 'a
  let int x = x
  let add e1 e2 = e1 + e2
  let mult e1 e2 = e1 * e2                    (* we can add it here *)
end

module InterpretAsString = struct
  type 'a repr = string
  let int = string_of_int
  let add x y = "(" ^ x ^ " + " ^ y ^ ")"
                                              (* and omit multiplication here *)
end

module Eval (I:Sym) = struct
  open I
  let my_ast = mult (add (int 1) (int 2)) (int 3)
end

module Num = Eval(InterpretAsNum)
let result_num = Num.my_ast
