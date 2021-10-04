type name
type constant
type var = string
type expr =
  | Var of var
  | Const of constant
  | Fun of var * expr
  | App of expr * expr
  | Let of var * expr * expr

val plus: expr
val times: expr
val int : int -> expr


