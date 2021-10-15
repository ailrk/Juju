# How to build a regex engine?

We aim to build the engine with the minimal amount of features: only cover everything in klenee algebra, so it exactly corresponds to regular language.

The plan is basically build a tree walk interpreter that interpret the regular expression dsl, and compiles to a 5 tuple dfa. The most important step is to build up the transition function delta; this can be achieved by building a table or a closure at runtime.

Also to build the interpreter we need to parse the dsl string of course. It's intereting that regex itself is context free grammar so we make a simple recursive decent parser for that.
