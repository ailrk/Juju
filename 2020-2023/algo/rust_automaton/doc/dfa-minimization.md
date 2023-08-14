# DFA minimization

Myhill nerode equivalence is used for minimize dfa. First recall nerode equivalence L~ means for a langauge L, two string x, y, ~∃z s.t xz ∈ L ∨ xy ∈ L, then these two strings are equivalant. They can be equally accepted.

L~ partition the language L into several equvalence class { x ∈ L | x L~ a }. Myhill nerode theory says L is regular iff L~ has finite number of equivalance classes, and the number of equivalance classes is the minimal number of states in the minimal dfa.

So how do we know if two states contains distinct strings we can merge them.

## Moore

## Hopcroft

## Rereferences
- http://www-igm.univ-mlv.fr/~berstel/Exposes/2009-06-08MinimisationLiege.pdf
- http://i.stanford.edu/pub/cstr/reports/cs/tr/71/190/CS-TR-71-190.pdf
