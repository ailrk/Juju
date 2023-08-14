
## Types of DSL:
   1. embeded dsl, langage is embedded within the host langauge, reuse host
      language features.

   2. external dsl, you design a entire new language with it's hole amenities.
      parser, compiler, editor, libraries. etc.

Algebraic data type, higher order functions are very useful to design embeded dsl. (For deep embedding and shallow embedding respectively).

For embedded dsl, we have further two classifications:
  1. deep embedding
        The abstract syntax of the target langauge is represented as data
        type in the host language, acompany with a eval function that
        interpret terms of the target languages.
```haskell
data ExprDeep :: * where
  Val :: Integer -> ExprDeep
  Add :: ExprDeep -> ExprDeep -> ExprDeep

evalExprDeep :: ExprDeep -> Integer
evalExprDeep (Val n)   = n
evalExprDeep (Add n m) = evalExprDeep m + evalExprDeep n
```

  1. shallow embedding
        Deeply reuse the host language's functionality, skip over the syntax
        tree and compute values directly. It's similar to make a set of
        combinators that works together as a dsl.
``` haskell
type ExprShallow = Integer
val_ExprShallow :: Integer -> ExprShallow
val_ExprShallow n = n

add_ExrpShallow :: ExprShallow -> ExprShallow -> ExprShallow
add_ExrpShallow a b = a + b
```

or tagless final sometimes called.

More jargons:
  deep embedding => initial approach because the AST corresponds to the
                    initial algebra
  shallow embedding => final approach

## Compositional interpretation (just folding)

##### reference
https://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/embedding-short.pdf
