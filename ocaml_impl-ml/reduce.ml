open Syntax
open Util

(* small step reduction semantics defined in redexes *)
exception Reduce

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

let top_reduction = union beta delta

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

