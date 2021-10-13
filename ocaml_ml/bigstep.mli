open Syntax
type env = (string * value) list
and value =
  | Closure of var * expr * env
  | Constant of constant * value list
type result = Error | Value of value
module type BigStepEval = sig val eval : env -> expr -> result end
module BigStep : BigStepEval
