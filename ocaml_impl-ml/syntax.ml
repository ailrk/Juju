  type name = Name of string | Int of int
  type constant = { name: name; constr: bool; arity: int }
  type var = string
  type expr =
    | Var of var
    | Const of constant
    | Fun of var * expr
    | App of expr * expr
    | Let of var * expr * expr

  let plus = Const { name = Name "+"; arity = 2; constr = false }
  let times = Const { name = Name "*"; arity = 2; constr = false }
  let int n = Const { name = Int n; arity = 0; constr = true }
