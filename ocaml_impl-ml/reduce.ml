open Syntax
open Util
exception Reduce

(* Reduction
   - operatonal semantics: relates programs to the result of their evaluation
     1. simple.
     2. too concrete, hard to prove some properties.

   - denotational semantics: map program to math structure (called domain)
     1. more abstract. Can prove hard properties very concisely.
     2. harder to establish.

    ML Call by value, definition of evaluated forms:
      v ::= λx.a
          | Cⁿ v₁...vₙ                -- constructed value
          | Cⁿ v₁...vₖ where k < n    -- partially applied constant

     - small step decution semantics
       - defined by a set of redexes.

         - first three rules for reduction (really only one rule (βᵥ))

         ------------------ (βᵥ)        ------------------------- (Letᵥ)
          (λx.e)v → a[x←v]               let x = v in a → a[x←v]

         ------------------ (fⁿ v₁ ‥ vₙ → a, a) ∈ δf
           fⁿ v₁ ‥ vₙ → a

          - and rules for call by value evaluation order.

              e₁ → e₁'                        e₂ → e₂'
          ---------------- (app-left)     --------------- (app-right)
           e₁ e₂ → e₁' e₂                   v e₂ →  v e₂'


     The last rule describes how to reduce primitives. (delta rules)
     e.g for plus +, δ+ = { (|p| + |q|, |p + q|) | p, q ∈ ℕ }, where |n| is
         the constatn representation of integer n.

     - refexes: partial function from programs to programs.
                (reducable expression)
 *)


(* check if a lambda expr is evaluated *)
let rec evaluated = function
    Fun (_,_) -> true
  | u -> partial_application 0 u
and partial_application n = function
    Const c -> (c.constr || c.arity > n)
  | App(u, v) -> (evaluated v && partial_application (n+1) u)
  | _ -> false

(* redex δf *)
let delta_bin_arith op code = function
  | App (App (Const { name = Name _; arity = 2; constr = _ } as c,
              Const { name = Int x; arity = _; constr = _  }),
              Const { name = Int y; arity = _; constr = _  } )
    when c = op -> int (code x y)
  | _ -> raise Reduce   (* redexes are partial *)
let delta_plus = delta_bin_arith plus ( + )
let delta_times = delta_bin_arith times ( * )
let delta_rules = [delta_plus; delta_times]

(* redexes is an instance of Alternative *)
let union f g a = try g a with Reduce -> f a

let delta = List.fold_right union delta_rules (fun _ -> raise Reduce)

(* call by value semantics means v is evaluated before app. *)
let rec subst x v e =
  assert (evaluated v);
  match e with (* dispatch around e[x←v]. *)
  | Var y -> if y = x then v else e
  | Fun (y, e') -> if y = x then e else Fun (y, subst x v e')

  | Let (y, e', e'') -> if y = x then Let (y, subst x v e', e'')
                                 else Let (y, subst x v e', subst x v e'')
  | App (e', e'') -> App (subst x v e', subst x v e'')
  | Const c -> Const c

(* beta reduction simply substitute on application and let. *)
let beta = function
  | App (Fun (x, e), v) when evaluated v -> subst x v e
  | Let (x, v, e) when evaluated v -> subst x v e
  | _ -> raise Reduce

let top_reduction = List.fold_right union [beta; delta] (fun _ -> raise Reduce)



module EfficientSmallStep = struct
  (* a recursive decent evaluator. It scans it's input only once.
   *)
  let rec eval =
    let eval_top_reduce a = try eval (top_reduction a) with Reduce -> a in
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


module SmallStep = struct
  (* the real small step semantics look like this
     Follows the reduction semantics closely.
   *)
  let rec eval = function
    | App (a1, a2) when  not (evaluated a1)-> App (eval a1, a2)
    | App (a1, a2) when  not (evaluated a2)-> App (a1, eval a2)
    | Let (x, a1, a2) when not (evaluated a1) -> Let (x, eval a1, a2)
    | a -> top_reduction a
end

module EvalContext = struct

  (* evaluation contexts
     A way to describe structural congurence rules (e.g CBV reduction rules)

     E[.] is an evaluation context describes a family of lambda terms with a
     special variable [.] called hole.

     If E[.] is an evaluation context, E[a] represent the context with the
     hole substituted with expression a.

   congruence rule for E[.]
         e → e'
      -------------
      E[e] → E[e']

    and back to 2 CBV structurl congruence rules, we can write it as ([.] e)
    and (v [.]). Thus the semantics can now be written as
      (λx.e)v → e[x←v]    [.] e    v [.]

    Bonous:
      CBN with evaluation context:
        (λx.e)v → e[x←v]    [.] e     -- always evaluate  body frist

     We can also describe the set of all evaluation context of CBV ML core
     with this:
        E ::= [.] | E e | v E | let x = E in a
 *)

  (* We can separate the small step reduction into three steps to expose the
     evaluation context:
       1. eval context to get back a context and a term
       2. reduce
       3. shove the evaluated expression back to the context
   *)

  (* Evaluation context is an one hole context, naturally we can represnet it
     with a zipper.
   *)

  module type ZIPPER = sig
    type v
    type context = v -> v
    val hole: context
    val appL: v -> v -> v
    val appR: v -> v -> v
    val letL: var -> v -> v -> v
    val ( ** ): ('a -> 'b) -> ('c -> 'a) * 'd -> ('c -> 'b) * 'd
  end

  module MLZipper : ZIPPER with type v = expr = struct
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

  let eval e = let c, t = eval_context e in c (top_reduction t)
end
