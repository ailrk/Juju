# Parser Survey

Parsers, at it's core is just a big predicate. It's only mission is to verify if a given text is valid for a langauge, and to it's convinence, maybe, create an AST for you.

Before writing a parser, the first thing to do is to have a clearly defined bnf. For a tool like yacc you're forced to write one anyway so it's not an issue. But when hand writing a parser, it's really painful to modify the entire parser in the middle of the process. With an finalized bnf at hand, everything is much easier to deal with.

Productioon rule for context free grammar itself is a domain specific langauge. Itself is a inductive definition, and interestingly, we can write a context free grammar in natural deduction style. This is an example langauge with paired parenthesis.

```
  S -> S S  | ( S ) | () | ε
```

If we say `n S` means n is in language S, the above production rules can be written as:
```
                         s S
  ------      ------    -------
   () S        ε S       ( s )
```

This looks exactly like how you define semantics.

PS: The principle of inductive definition is a set of rules works on a set of cases. Once in action, given input matches on cases until hit some base case. Inductions like peano nuatrual numbers is simple as it only have two cases, but nothing stops us to have a system with like, 1000 cases, and that is what we have here.

Paired parenthesis is the essense of context free grammar! To parse something like this, you need to be able to remember. When you hit a ) you need to know whether there were a ( parsed eariler. This is only possible with context free grammar. A regular langauge only supports repeatation linearly, paired parenthesis is way to fancy for it.

#####  Top down parser

Top down parsers build the ast with preorder traversal. During parsing, it first try to parse the top level rule, then going down non terminals recursively until it hits teriminals. Thus the name `recursive descenet`.

A top down parser is easy to write, we can simply write each non terminal as a function, a function can call other non terminals, until a character parser is is hit. A top down parser can bascially be transliterated from this bnf direclty.
```
  S -> E                        void expr();
  E -> E + T                    void term1();
     | E * T       ->           void term2();
     | T                        // just match on characters.
  T -> <number>
```

A top down parser either needs back tracking, or it doesn't need back tracking. Back tracking happends when the parser needs to fail in order to know it made the wrong assumption, and the next node is of some other types. In bad cases a back tracking parser needs to 1. parse almost the entire text and find it's some other type, discard all it's work and try with some other rules; 2. try large amount of rules, none of them works, because the last one is the right one.

```
A backtracking requried grammar
  S -> rXd | rZd
  X -> oa | ea
  Z -> ia
```

Left recursion is a specialized back tracking problem. The problem happens simply because some nonternimals needs to parse itself for the left most term so it ends up in infinite recursion.
```
 S -> rXd
 X -> Sa | b
```

If the grammar doesn't require back tracking, the parser is predictive. That is, it can determine which rule to use by look k tokens ahead without trying each rules in turn and back track if failed. Grammars can be parsed in this manner are sometimes referred as LL(k) grammar. It's weird naming becasue it defines a type of grammar with it's parsing mechanism which one would expect it parser some types of grammar. It's very circular to me.

Anyway, we can factor out a grammar with left recursion into a LL(k) grammar, but if the gramamr is not LL(k) then we have to backtrack.

```
        Top Down
           |
     Recursive Descent
         /          \
  Back Tracking     Predictive Parser
                     |
                     LL(k)
```

##### Bottom up parser

Bottom up parser generate the parse tree with a post order traversal. This means the AST node is the last one to be decided. It's a very important property, becasue once we are making the decision, we already see all tokens for the node.

A bottom up parser typically has two steps: shift and reduce, so it's also called a shift reduce parser (LR parser). It keep shifting symbols from the token stream onto the stack until it finds the current content on the stack matches some grammar rules. Once that happen, it will reduce everything in to one node and add into the parse tree.

Good things: there is no recursion on LR parser, so no need to worry about memory usage. The shift reduce mechanism is pretty mechanical, pretty much just a while loop, shifting, checking the content on the stack, reduce if some rules match.

```
        Bottom up
            |
     Shift Reduce Parser
            |
            LR
       /     |     \
     SLR    LR     LALR
```

##### Parser combinator
Is just a top down parser, nothing special about it. In haskell you have monadic and applcative interfaces (well, just do notations and much of <*> <$> thingys) to combine things so the final fuction `looks like` a production rule but it isn't. You still pretty much need a bnf.

A parser is a monad, it's not surprising at all, everything is a monad anyway. (Again not suprising that it parses context sensative grammar since within monad you're essentially writing unrestricted imperative code, just like how you're writing parser in, say, C).

But it is interesting to know correlations like applicative parser with context free grammar and monadic parser with context sensative grammar.

##### Best way to parse anything
Is just to use a parser generator really...
