# ML semantics

(*****************************************************************************
   Reduction
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
     - σ reduction describes how to reduce primitives.
     - refexes: partial function from programs to programs.
                (reducable expression)
 *)
    (* Follows the reduction semantics closely.
         rules for call by value evaluation order.
              e₁ → e₁'                        e₂ → e₂'
          ---------------- (app-left)     --------------- (app-right)
           e₁ e₂ → e₁' e₂                   v e₂ →  v e₂'
     *)

(*****************************************************************************
  reduction rules for call by value semantics, v is evaluated before app.
  ---------- (βᵥ) --------------- (Letᵥ) --------------- (fⁿ v₁‥vₙ→a,a)∈δf
  (λx.e)v→e[x←v]   let x=v in e→e[x←v]         fⁿ v₁ ‥ vₙ → e
  Practical langauges need both beta and segma reductions.
 *)

(*****************************************************************************
  evaluation contexts
       A way to describe structural congurence rules (e.g CBV reduction rules)
       E[.] is an evaluation context describes a family of lambda terms with a
       special variable [.] called hole.
       If E[.] is an evaluation context, E[a] represent the context with the
       hole substituted with expression a.
     congruence rule for E[.]
           e → e'
        -------------
        E[e] → E[e']
      back to 2 CBV structural congruence rules, we can write it as ([.] e)
      and (v [.]). Thus the semantics can now be written as
          (λx.e)v → e[x←v]    [.] e    v [.]
      Bonous CBN with evaluation context:
          (λx.e)v → e[x←v]    [.] e     -- always evaluate  body frist
       We can also describe the set of all evaluation context of CBV ML core as
          E ::= [.] | E e | v E | let x = E in a

    We can separate the small step reduction into three steps to expose the
    evaluation context:
         1. eval context to get back a context and a term
         2. reduce
         3. shove the evaluated expression back to the context
   *)

(****************************************************************************
   Big step semantics
   small step semantics (reduction semantics) is concise and modular. But it
   has some drawbacks:
     1. values are subset of programs
     2. poor performance.
  Introducing big step sematnics: program evaluated in an environment. We
  can jump to conclusion instead of following reduction steps precisely. This
  makes big step semantics less interesting in a theoritical point of view.
    ρ := ∅ | ρ, x → v
    v := ⟨ λx. a, ρ⟩ | Cⁿ v₁‥vₙ | cⁿ v₁‥vₖ where k < n
    r : v | error
  Big step semantics are defined by a set of inference rules that the language
  has. Some inferences can lead to errors.
  Actually we can categorize rules in to 3 groups:
    1. eval rules
    2. error rules
    3. error propagatoin

  ρ ⊢ a ⇓ v                 ρ ⊢ a ⇓ error                    ρ⊢a⇓V  f¹ v→v'
-------------eval-const  -------------- eval-const-error  ------------eval-prim
 ρ ⊢ C¹ a ⇓ C¹ v           ⊢ c a ⇓ error                    ρ⊢f¹ a ⇓ v'
 ρ⊢a⇓V    f¹ v→~v'             z∈Dom(ρ)
---------------eval-error   -------------eval-var  ------------------ eval-fun
    ρ⊢f¹ a ⇓ error            ρ ⊢ z⇓ρ(z)              e ⊢ λx.a ⇓ ⟨λx. a, ρ⟩
    ρ ⊢ a ⇓ ⟨λx.a₀, ρ₀⟩     ρ⊢a'⇓v    ρ₀, x→v⊢a₀:v'
   ------------------------------------------------ eval-app
                ρ⊢ a a' ⇓ v'
    ρ⊢a ⇓ C₁ v₁                         ρ⊢a⇓ error
   -------------- eval-app-error     ---------------- eval-app error left
   ρ⊢a a' ⇓ error                       ρ⊢a a' ⇓ error
          ρ⊢a⇓⟨λx.a₀, ρ₀⟩         ρ⊢a' ⇓ error
   ---------------------------------------------- eval-app-error-right
              ρ⊢a a' ⇓ error
   ρ⊢a ⇓ v     ρ, x→v ⊢a'⇓v'                  ρ⊢a ⇓ error
------------------------------ eval-let    ----------------- eval-let-error
    ρ ⊢let x = a in a' ⇓ v'                ρ⊢let x = a in a' ⇓ error
 *)

 (* Static semantics of core ml
 *)

(* Prove well typed programs never go wrong
        1. δ reduction preserve typings
        2. reduction preserves typings
        3. δ reduction is well defined .
        4. programs are well typed in initial env, they can either value
           or can abe further reduced
 *)


(* - type inference: Given type env A, term a and type τ, find all substitition
    θ such taht θ(A) ⊢ a  : θ(τ).

  - principal types:
*)

#### Reference
https://caml.inria.fr/pub/docs/u3-ocaml/index.html
