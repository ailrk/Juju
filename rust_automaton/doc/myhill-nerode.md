# Myhill nerode theory

Myhill nerode theory is another theory that can be used to check regular language. Comparing with pumping lemma, it's more powerful in terms that it can identify non langauges that pummping lemma cannot.

The key insight: each state in a dfa represents a set of string that it accepts, which can be thought as an equivalence class. If we have infinite many equivalence classes, then we have inifnite many states, but regular languguage only have finite states. Thus contradict.

To check wheather a langauge is regular we can try to construct inifinte many equivalence classes.

The idea of states being equivalence relation is useful in many other areas. If we know two states are in the same equivalence class, we call them `non distinguishable states`, thus we can eliminate one of it to make the dfa smaller. This is one important step to generate `canonical dfa (minimal dfa)`.

__distinguishing extension__:
given language L, for string x, y if there exists a string z such that xz in L or yz in L.

__relation L~__: x L~ y iff there is no distinguishing extensions z.

- L is regular iff L~ has finite number of equivalence classes.
- number of L~ is the minimal dfa.
- minimal dfa is unique.
- two string, if there is a distinguishing extension, they are not equivalent.

#### Some lemmas

__lemma1__: for a regular langauge L, if x, y going to the same state, then xz, yz going to the same state.

```
        x
      +--------+
  () -+        + --> () ----> z
      +--------+
         y
```
__lemma2__: if xz, yz goes to same state, x, y either

__lemma3__: if xz, yz don't go to same state, neither x, y

NOTE: if xz, yz don't go to same state, they go to some different states.

__lemma4__: if infinitely many distinguishing extension z such that forall x ≠ y, xz, yz go to different state, L is not regular.

__Proof__: First note that xz, yz go to to different states. If we have infinite many z that xz, yz goes to different states, by lemma4 x, y must have gone to different states.

Lets assume xz goes to state Sx, yz goes to state Sy. For inifinite many z, we have infinite many pairs of (Sxᵢ, Syᵢ) that Sxᵢ ≠ Syᵢ where i ∈ ℕ. That means we need infinite number of states. But regular languages have finite states, absurd ∎ (Lemma4)

#### each State in dfa corresponds to an equivalance class
Each state in dfa represents a set of strings that can be accepted. All these string can be thought to be equivelant to each other. For language `10(10)*|11(11)*`, we have x ≡ 10 and x ≡ 11 two equivalence classes, accepting `101010...` and `111111...` respectively.

#### Pummping lemma can't prove L = a* ∪ { aᵐbⁿ : m > n }
It's a non regular langauge that pummping lemma can't prove.

Pummping lemma says a regular langauge satisfies: for a regular language L, there exists a integer p such that for all string w in L, we can write the string like xyz, that |y| > 1, |xy| ≤ p, and forall n ≥ 0, xyⁿz in L.

So to prove something is a regular language, we need to use the contrapositive of the lemma, that is, if a langauge does not satisified aforementioned conditions, it's not a regular langauge.

But we don't have a proof for all non regular langauges don't satisfy the condition, thus there exists languages that are non regular, but still have the pumping property.

The title is an example for such case. Intuitively we know it's not regular, becuse we can't express the language with regular expression. (How do you express a substring is longer than it's suffix?) But we can't prove it formally with pumping lemma.

Instead, to prove it's not regular, we can use myhill-nerode relation. It's another way to define a regular language. Once we define the relation, we know a regular language has only finite number of equivalence classes number the relation L~. If we can prove there are infinite many equivalant class for the language L, we can prove it's non regular.

#### Prove L = {0ⁿ1ⁿ : n≥0} is non regular with myhill nerode
Why not the previous example? Because this is easy :0

Let Z = 0* = {0, 00, 000, ...}

let x = 0ⁱ, y = 0ʲ, where i ≠ j. let z = 1ʲ. So xz = 0ⁱ1ʲ ∉ L, yz = 0ʲ1ʲ ∈ L. Thus there are infinite many distinguishing extension z ∈ Z such that only one of xz and yz in L. Thus L is non regular by lemma(4). ∎


#### Application: merging nondistinguishable states

Two states are non distinguishable if they behaves the same for all input. In another word, they accepts the same set of strings. With our previous definitions we can tell two states are nondistinguishable if they are in the same equivalence class.

So to remove nondistinguishable states meaning merging states in the same equivalence class into on state.


This is important because we need to do this to get minimize (canonical) dfa.

#### Algorithm for getting Canonical dfa

A dfa can have unecessary states. We alreay discussed that it can have nondistinguishable states falls in the same equivalance class. Also there will be states can never been reached from the initial state. Removing all theses states we can get one unique canonical form of the dfa that accept the same langauge.

To eliminate unreachable nodes, we maintain a set of reachable nodes. At the beginning the set only have initial node. Each step we check the next states from the current reachable states (by applying delta to each reachable state and all symbols in the language). If it's a new state, we add it into the set. Repeat until there is no more new states. (For performance, we can add the concept front state, which are only new states found from the last iteration, thus we don't need to check all reachable states)

To eliminate nondistinguishable states, we perform parition refinement on states, which shoves states in the same equivalance class into the same partition, then merge all states in the same parition.
