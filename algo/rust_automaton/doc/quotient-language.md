# Quotient langauge

Let L₁, L₂ be two langauges, we define L₁ / L₂ ≡ { x | ∃y ∈ L₂ s.t xy ∈ L₁ }

__[thm]__ R be regular language, L be any language, then R/L is regular.

It's saying that if we have a regular langauge R and a whatever language L, and picking string from the regular langauge that as long as there is a suffix of a string in x exists in L, we pick it into the new quotient language, then the new quotient langauge generated must be regular.

It's interesting because we don't really know what the quotient langauge is like, but we can be sure the generated langauge is regular for sure.

__Proof__:
To get R/L, let M be a DFA for R. We can change final states of M to be all states q s.t if we read some string y∈L and end in a original final state.

For x ∈ R, if ∃y∈L, such that xy ∈ R and xy hits the final state of R, let's say x hit a statet called q, then we know first x needs need to be accepted in langauge R/L, that means q needs to be the final state in R/L.

Another way to look at it is that L only chop off y part of the string in xy from hehind, leave the first part x which is itself in a regular language. The resulting langauge R/L now accept a smaller string x instead of xy, but the part of automata M from the initial state q0 up to the state that accepts x is still regular.
