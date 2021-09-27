(* big step semantics / natural semantics
   one step reduction is not very efficient, because each reduction
   step is not constant time.

   In small step reduction each reduction step we need to:
      find the next redex -> substitute -> reconstruct the term
   GOAL: We want to amortize the cost over whole reduction sequence.

    a b ->* (\x.c) b ->* (\x.c) v' ->* c[x <- v'] ->* v
       ^            ^
    a ->* \x.c    b ->* b'

    # Big step semantics:
    define relation a => v means a evaluates to value v.

    PS:
    CBN (value cases are the same as CBV)
      a => \x.c   c[x <- b] => v
     --------------------------------------
              a b => v
 *)

module BigStepSemantics = struct
  exception Error

  type term = Const of int
            | Var of string
            | Lam of string * term
            | App of term * term

  let isvalue = function
    | Const _ -> true
    | Var _ -> true
    | _ -> false

  let rec subst x value = function
    | Const  n -> Const n
    | Var n -> if x = n then value else Var x
    | Lam(arg, body) -> if x = arg then Lam(arg, body)
                                   else Lam(arg, subst x value body)
    | App(lam, arg) -> App(subst x value lam, subst x value arg)

  (* we no longer need to search for redex and reconstruct reduced redex
    CBV big step semantics:
        N => N            \x.a => \x.a

      a => \x.c   b => v'  c[x <- v'] => v
     --------------------------------------
              a b => v
   *)
  let rec eval = function
    | Const n -> Const n
    | Var _ -> raise Error
    | Lam(x, a) -> Lam(x, a)
    | App(a, b) ->
        let va = eval a in
        let vb = eval b in
        match va with
        | Lam(x, c) -> eval (subst x vb c)
        | _ -> raise Error
end
