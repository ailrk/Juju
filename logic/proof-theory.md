# Proof theory

```
Logic Trinity: (Axiomatics, Syntax, Semantics)
```

## Proof calculi
A proof calculs is a proof system usedto prove statment. A proof calculi contains:

1. Language (Set of admitted formulas)
2. Rules of inference (list of rules that can be employed to prove theorem from axioms and theorems)
3. Axioms (Formulas that assumed to be valid)

Idea is to prove new knowledge from known facts, all propositions are derived from axioms.

The same proof calculs can be used for many very different logics. Sequent calculus can be used for describing first order logic, linear logic, intuitionistic logic etc. The reason is essentially that logic about inference, and proof calculus provides a framework for doing inferences.

#### Natural deduction
- First structural proof theory
- Express inference in a "natural" way instead of heavily rely on axioms (not like Hilbert style system).
- Provide a framework for doing decuctive reasoning.
- `Judgement`: A judgement is evident if one has a proof for it.

Some flavors of Judgements:
1. A is true (basic for all logics)
2. A is a proposition (this is implicit for A being true)
3. A is false (classic logic with exluded middle)
4. A is true at time t (temporal logic)
5. A is necessarily true / A is possibly true (modal logic)
6. Program M has type r (type theory)
7. A is achievable from given resources (linear logic)


```
General Form:
  J₁   j₂  ‥  Jₙ
------------------- name
       J
```

```
An example of a set of inferences.
  A prop    B prop            A prop    B prop
-------------------- ∧F   --------------------- ⊃F   -------⊤F   --------⊥F
     (A ∧ B) prop                A ⊃ B prop           ⊤ prop      ⊥ prop
```

#### Sequent calculus
