open Syntax
let plus = Const { name = Name "+"; arity = 2; constr = false }
let times = Const { name = Name "*"; arity = 2; constr = false }
let int n = Const { name = Int n; arity = 0; constr = true }
