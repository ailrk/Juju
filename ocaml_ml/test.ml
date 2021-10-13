open Syntax

(* NOTE
   1. both primitive binop and values are represented with const. For binops
      we still need to apply. Thus to add two numbers we do something like
        apply (apply (const + ) (const 1)) (const 2)
 *)
let e1 =
  let plus = Const { name = Name "+"; arity = 2; constr = false } in
  let times = Const { name = Name "*"; arity = 2; constr = false } in
  let int n = Const { name = Int n; arity = 0; constr = false } in
  let plus_x n = App (App (plus, Var "x"), n)
  in App (Fun ("x", App (App (times, plus_x (int 1)), plus_x (int (-1)))),
          App (Fun ("x", App (App (plus, Var "x"), int 1)),
               int 2))
