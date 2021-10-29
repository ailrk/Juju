# 2021-05-28 [Theorem for free, Philip Wadler, 1989]

### Intro

The paper is about `parametricity`, a property of polymorphism, and how it helps us to reason about polymorphic types. Parametricity is the theorem behind polymorphism, it tells you what polymorphism really is, what can it obtain, what it can't do.

In short, parametricity means you cannot pattern match on type parameter, so polymorphic fundtions or polymorphic data types performs uniformly on their instantiations. A simple example is `map (+1) . reverse ≡ reverse . map (+1)`, where `map (+1) :: [Int] -> [Int]` and `reverse :: forall a. [a] -> [a]`. Here reverse only act on the outer list type and knows nothing about what a is. An counter example of parametricity is `all :: forall a . Eq a => [a] -> Bool`, where a only works for types being instance of `Eq` class. The typeclass constrain impose an implicit pattern maching that accept certain types and reject others.

Parametricity it's a strong property on polymorphic values, it allows us to derive some invariants for free.

### Parametricity
In a lambda calculus based language with parametric polymorphism (e.g system F), a polymorphic value can be expressed as `forall a. τ`. There is no constrain on what type a will be, the type behaves uniformly over all a. So we can write a general implementation to encoded all possible instantiation. For example, the id function can be written as `id :: forall a . a -> a; id = \a -> a`, but in fact the id function for `String` and id function for `Int` are two different functions. `idInt :: Int -> Int`, and `idString` are two members in the family of functions defined by the lambda calulus encoding.

it's easy to write down the general representation of polymorphic functions, but it's hard to write it for arbitrary polymorphic values. For example, for type `x :: forall a . (a, a, a)`, to write the corresponding term level value, you need to be able to provide all possible triple of all type a. It's not only hard to do, it's impossible. Because there exists types that are not inhabitated at all. How do you write an implementation for x when a is `Void`? On the other hand, you can write down the implementation for type `y :: forall a . a -> (a, a, a); y = \a -> (a, a, a)`. The form of the function works the same over all a, and what a will be depends on how it will be used.

In system F, we need to introduce polymorphic types with their type parameter explictly. That's what `forall` is for. In the context of system F, it's only used as an introduction of parameters, and then meaning of the quantifier is meaningful only under the context of curry howard isomorphism. It's convinent to think it as a type level lambda, and a polymorphc type is a type level function. In this pov, not only `data List a` is a polymorphic type, `forall a . a -> a` is also one, representing a function with the type. We can read the type signature as "a function that for all a, a to a". This idea is an extension of function as data to the type level.


### Set theorical point of view on polymorphism

There are two possible ways to think about types with set theory. The naive approach is to view types as sets, and functions between types as functions between sets. Functions themselves forms a set. Under this assumption, a polymorphic type is a polymorphic set `forall a. b`, which takes a concrete set to form another concrete set.

Another way to think about types in terms of sets is to treat them as relation. Relation is a general version of functions,

Once we're able to represent the correspondence between types as relations, we are able to express the behavior of functions.

### Example


### Some examples of parametricity
Assume functions `a :: a -> b; b :: b -> c`, we have

Taking the head of a list and then update the value is the same as update all elements in the list then taking the head.
```haskell
head :: forall a . [a] -> [a]
-- >>> a . head ≡ head . fmap a
```

```haskell

```



### Historical remarks


### Real world
SML is all about parametricity, and there is no good way to do adhoc polymorphism, which make the type system sometimes less expressive. Subtype polymorphism to some extend is also adhoc polymorphism, but for values to be polymorphic it must form a subtype relation with it's super type. The same function name is overloaded at call site.



### Reference
- https://people.mpi-sws.org/~dreyer/tor/papers/wadler.pdf
