# Myhill nerode relation

__[distinguishing extension]__:
given language L, for string x, y if there exists a string z such that xz in L or yz in L.

__[relation L~]__: x L~ y iff there is no distinguishing extensions z.

- L is regular iff L~ has finite number of equivalence classes.
- number of L~ is the minimal dfa.
- minimal dfa is unique.
- two string, if there is a distinguishing extension, they are not equivalent.

#### Some lemmas

[lemma1]: for a regular langauge L, if x, y going to the same state, then xz, yz going to the same state.

```
        x
      +--------+
  () -+        + --> () ----> z
      +--------+
         y
```
[lemma2]: if xz, yz goes to same state, x, y either
[lemma3]: if xz, yz don't go to same state, neither x, y
[lemma4]: if infinitely many z such that forall x ≠ y, xz, yz go to different state, L is not regular.

#### Pummping lemma can't prove L = a* ∪ { aᵐbⁿ : m > n }
It's a non regular langauge that pummping lemma can't prove.

Pummping lemma says a regular langauge satisfies: for a regular language L, there exists a integer p such that for all string w in L, we can write the string like xyz, that |y| > 1, |xy| ≤ p, and forall n ≥ 0, xyⁿz in L. o

So to prove something is a regular language, we need to use the contrapositive of the lemma, that is, if a langauge does not satisified aforementioned conditions, it's not a regular langauge.

But we don't have a proof for all non regular langauges don't satisfy the condition, thus there exists languages that are non regular, but still have the pumping property.

The title is an example for such case. Intuitively we know it's not regular, becuse we can't express the language with regular expression. (How do you express a substring is longer than it's suffix?) But we can't prove it formally with pumping lemma.

Instead, to prove it's not regular, we can use myhill-nerode relation. It's another way to define a regular language. Once we define the relation, we know a regular language has only finite number of equivalence classes number the relation L~. If we can prove there are infinite many equivalant class for the language L, we can prove it's non regular.

#### Prove L = {0ⁿ1ⁿ : n≥0} is non regular with myhill nerode
Why not the previous example? Because this is easy :0

Let Z = 0* = {0, 00, 000, ...}

let x = 0ⁱ, y = 0ʲ, where i ≠ j. let z = 1ʲ. So xz = 0ⁱ1ʲ ∉ L, yz = 0ʲ1ʲ ∈ L. Thus there are infinite many distinguishing extension z ∈ Z such that only one of xz and yz in L. Thus L is non regular by lemma(4). ∎
