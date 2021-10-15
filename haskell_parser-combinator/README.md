# Parser combinator

Parser combinator is another way to write recursive descent parser. The idea is similar to normal recursive descent parser: writing functions to represent production rules. On top of that, you compose small parsers with a set of well defined combinators, which make parsers a little nicer to work with.

A parser is a predicate. It takes a string, and decide whether to accept or reject it. To be useful, we also want it to perform some semantic actions. For example to do things like whenever it encounter a certain pattern, process the pattern and return us some data structure.

Recursive descent parser means we breakdown a large parser into several smaller one, each one accept a tiny little language. Because a single parser can't parse the entire langauge, it should consume part of the input string, and return the rest that it doesn't recognize to hand it over to other parsers.

Combine two points mentioned above, we want something works like `s -> (s, a)`. This is exactly like a state monad. We work our way on this direction, make parsers a monad, so we can use monadic interfaces and applicative operators to combines things.

```haskell
data PResult a = PError PError | Result Input a
newtype ParserT m a = ParserT {parseT :: Input -> m (PResult a)}
```

We can also define our parser as a little dsl by defunctionalize some critical operations, and write an interpreter to run the parser. Instead of thinking a parser as a pushdown automata, we pick operations we find handy for parser and defunctionalize them into a algebraic data type, call it `P a`. A parser can get the current character, look head, it can success or fail, and eventually it will terminate. We encode this primitive operations and get a little instruction set.

```haskell
data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final (NonEmpty (a, String))
  deriving Functor
```

One instance of `P a` only represent one step to perform. To see how we compose things, note `Get (Char -> P a)`. In our little interpreter, whenever we match a Get, we can pass a char to it's function and get another `P a` that we can interpret again, this happens inductively until Fail or Final happens, We can manually compose two `P a` instances this way, but it's better to abstract it away as a monad.

```haskell
instance [Monad](Monad) P where
  (Get f) >>= k           = Get $ f >=> k
  (Look f) >>= k          = Look $ f >=> k
  Fail >>= _              = Fail
  (Result x p) >>= k      = k x <|> (p >>= k)
  (Final (r :| rs)) >>= k = final [ys' | (x, s) <- (r:rs) , ys' <- run (k x) s]
```


It's nature to do such a thing because (Char -> P a) itself is a monadic function, and our monad instance just compose it with another monadic function with klesli arrow.

There are some other operations we need to define to make the parser work. Namely making it Alternative. It's the semantic from the original klenee algebra that a parser also needs to be equipped with. The implementation is just stating what will happen when the permumation of two instances of `P a` meets together.

P a dsl, how do we interpret it? as we mentionsed, the core of a parser is still a state monad `String -> (String, a)`. If we allows to parse ambiguiously then one string can correspond to multiple result. the type will become `String -> [(String, a)]`, let's call it `ReadS`. We already know how to interpret `ReadS`, so for our dsl we can actually "compile" to ReadS for further evaluation.

```haskell
run :: P a -> ReadS a
run (Get f) (c:cs) = run (f c) cs
run (Look f) s     = run (f s) s
run (Result x p) s = (x, s) : run p s     -- run result appends result
run (Final rs) _   = NonEmpty.toList rs   -- parser end
run _ _            = []
```

We already seen the core of the type `P a` is to interpret defunctionalized `a -> P b`. A very interesting thing to observe: the Get itself is also a function, and it has the type `(a -> P b) -> P b`, which is cps transformed version of the value a. Now look at the monad instance, we are really building up a giant continuation to produce the final result.

We can abstract the notion into another type `newtype Parser a = R (forall b. (a -> P b) -> P b`. It contains a type similar to Get constructor in P. But becasue it's polymorphic over all b, we can use it to accomonadate all branches including Fail, Final, and Result, to create an uniform wrapper. We just call it parser becasue it bascially encodes everything we need.

We need to implement monad, alternative and other instances for `Parser a`. Becuse it's just a wrapper over `P a`, we have most thing done already, and the rest is just forwarding.

An example shows how this wrapper help us.

```haskell
get = R Get
look = R Look
pfail = R (const Fail)
```

Now all defunctionalized constructors reintroduced as combinators, we can compose them all under the same type. If we work with P, for case like pfail we need to manually write const Fail everywhere.

Once we build up a big Parser, how do we run it? It's a wrapper of a continuation function, so for it to return a value we only need to pass an id, in this case because it's a monadic function we pass a return instead.

```haskell
readPtoS :: Parser a -> ReadS a
readPtoS (R f) = run (f return)
```

It's a very nice little implementation, some good examples of cps, defunctionalization, and how to build up combinators for dsl.
