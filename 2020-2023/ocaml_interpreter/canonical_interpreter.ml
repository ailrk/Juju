(* A real interpreter needs to be efficient, we already seen interpreter
   base on how big step semantics has better performance then the one base
   one one step reduction rules. Here we try to apply some other techniques
   to make a relative ok interpreter.
 *)

(* 1. Address an environment instead of substitution.
    We have seen that for both big step and small step semantics variable
    binding can be achieved by simple text substitution a[x <- v].

    But this is very inefficient. Given how substitution is defined, we need to
    recurse over the entire tree.

    Another way to do it is to simply store the value bind with the parameter
    in a dictionary, called environment. Then every access of the paramter
    will be translated into a table lookup.

      written as
        e(x) = v
      --------------
       e |- a => v

    It's similar with closure conversion.
 *)

(* 2. Scoping
      Dynamic scoping is easy to implement, because it's the default behavior.
      On the other hand, if you want lexical scoping, you need to consider
      problems like closures. Because free variables is textually close to
      the call side, but in memory they might be complete separated.

      a tuple of lambda and an environment (dict) to free variables
          closure: (\x.a)[e]
      is a closure. With lexicial scoping, closure completely takes over the
      role of plain lambda term.
 *)

(* De bruijn index to avoid name capturing.
   One problem with lambda calculus is that we can have name capturing. e.g
   two variables conflcts when doing application.
   One way to solve this is to use de bruijn index: a variable is assignmned
   with the number of nesting from the variable itself to it's binder. e.g
      \x. (\y. y x) x
               1 2  1
   x in \x. (\y. y x) .. is two layers away from the binder, we call it 2.
   This gives each variable a unique index within a lambda body.

   When dealing with free variables, we can simply use numbers that exceed the
   deepest nesting level. Note there is no real free variable as all free
   variables eventually get binds to some variable in outer scope.

   e.g when we inline a function into the body of another lambda, de burijn
   indexes in the function body may not be correct, but it doesn't matter since
   we can adjust them.

   Note lambda calculus are curried by default, syntax like \x y z is just
   a sugar, de burijn index is unambiguious.
 *)

(* Natural semantics with environments and closures
      term    a := N | n | \.a | a1 a2
      value   v := N | (\x.a)[e]
      env     e := {x1 -> v1; x2 -> v2 ...}

      e(x) = v
     -------------      e |- N => N     e |- \x.a => (\x.a)[e]
     e |- x => v

        e |- a => (\x.c)[e']   e |- b => v'   e' + (x -> v') |- c => v
     -------------------------------------------------------------------
                          e |- a b => v
 *)


module CanonicalInterpreter = struct
  exception Error

  (* note lambda no longer has paramter, all the binding mechanisms are off
     loaded to the environment*)
  type term = Const of int
            | Var of int
            | Lam of term
            | App of term * term

  (* use de burijn index, we don't even need a dictionary. The index
     gives us the mapping.
   *)
  type 'a environment = 'a list
  type value = Vint of int | Vclos of term * value environment

  let rec eval e a =
    match a with
    | Const n -> Vint n
    | Var n -> List.nth e n
    | Lam a -> Vclos(Lam a, e)
    | App(a, b) ->
        match eval e a with
          | Vclos(Lam c, e') ->
              let v = eval e b in eval (v :: e') c
          | _ -> raise Error
end

(* a pov
   with environment we still do substitution, but it's done in a parallel
   manner.
 *)
