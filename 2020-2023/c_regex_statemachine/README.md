# Regex engine compile to a table.

Every regular expression corresponds to a deterministic finite automata (DFA). A DFA essentially is a directed graph that each vertex is labeled by type "State", and each edge is labeled by "Symbol". In addition, all DFA has an initial state and a set of final state we can go to. Formally we can define a DFA as a five tuple: `(Q, ∑, q₀, δ, F)`.

What makes a DFA alive is the transition function delta. δ: ∑ × Q → Q. At each moment we look at our current state and the symbol, and delta tells us what next state we should go to. There are lots of way to think about it. You can think of a state as it's own equivalence class with all strings that that state can accept. (A state is an equivalence class, two states accept the same set of string are the same state.)

A regular expression engine is a regular expression to state machine compiler. We want to find a a way to build up the delta function at runtime. One way to do this is to create nesting closures, everytime we add a new transtion, we wrap over. On call, we check if we know how to handle the input parameter. If we do, handle it. Otherwise, we pass it to the next layer. Another way is to compile the state machine into 2d table indexed by the current state and current symbol. This is simple and powerful, but also very wasteful. The best way to implement delta function maybe a hashmap, but we want to keep it simple.

An interesting realization: arrays are curried functions, fully applied nd array gives us a mapping indexed by n parameters. To represent a delta function we need two dimensions, one for state and one for symbol. Symbols are just characters from string, and states are just integers. We are free to add new state into the table, and we can simply increment it to make state names unique. The value we get is the next state we go to.

The syntax for regular expression is very simple. The following is a ebnf factored out left recursion. It's interesting that regular expression itself is writtent in a context free grammar. I think there is no way to write down regex in regular language anyway, because you need to express recursive construct like an expression somehow. This means we need a little parser to parse the syntax.

```
factor = char | "(" expression ")"
term = factor ["*"]
term1 = term { term }
expression = term1 { "|" term1 }
```

To make things simple, this is a tree walk interpreter. Once the parser build the ast, we walk over the tree with preorder traversal, and modifying the 2d array according to what we see along the way. Once the table is build, it's trivial to check if it accepts a string. All we need to do is to trace over the mapping and see if we end up in one of the final state.

One optmization we can do is to do a dfa minimization to reduce the table to it's minimal size. We know any dfa has an unique minimal dfa, this is due to myhill rehode relation: Number of states in the minimal dfa is the number of equivalence classes. And we can merge nondistinguishable states that are in the same equivalence class.
