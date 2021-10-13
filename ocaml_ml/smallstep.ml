open Reduce
open Syntax

module type SmallStepEval = sig val eval : expr -> expr end
module type Eval = SmallStepEval

module Stepped : Eval = struct
  let rec eval = function
    | App (a1, a2) when not (evaluated a1)-> App (eval a1, a2)
    | App (a1, a2) when not (evaluated a2)-> App (a1, eval a2)
    | Let (x, a1, a2) when not (evaluated a1) -> Let (x, eval a1, a2)
    | a -> reduce a
end

module RecDecent : Eval = struct
  let rec eval =
    let eval_top_reduce a = try eval (reduce a) with Reduce -> a in
    function
      | App(a1, a2) ->
          let v1 = eval a1 in
          let v2 = eval a2 in
          eval_top_reduce (App (v1, v2))
      | Let(x, a1, a2) ->
          let v1 = eval a1 in
          eval_top_reduce (Let (x, v1, a2))
      | a -> eval_top_reduce a
end

module EvalContext = struct
  module MLZipper = struct
    type v = expr
    type context = v -> v
    let hole = fun t -> t
    let appL a t = App (t, a)
    let appR a t = App (a, t)
    let letL x a t = Let (x, t, a)
    let ( ** ) e1 (e0, a0) = (fun a -> e1 (e0 a)), a0
  end
  open MLZipper

  let rec eval_context: expr -> context * expr = function
    | App (a1, a2) when not (evaluated a1) -> appL a2 ** eval_context a1
    | App (a1, a2) when not (evaluated a2) -> appR a1 ** eval_context a2
    | Let (x, a1, a2) when not (evaluated a1) -> letL x a2 ** eval_context a1
    | a -> hole, a

  let eval e = let c, t = eval_context e in c (reduce t)
end
