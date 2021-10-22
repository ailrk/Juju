# Reflection in haskell

The implementation actually entails quite a lot of type level features, I thought it's just simply an api for exitenstial type.

Functions like `eqTypeRep` and `splitApps` are essentailly holes in GHC, We don't really have control over their implementation. But `Dynamic` is just an ordinary library.

The design principle of reflection is that making `Typeable` primtive, and implementing `Dynamic` on top of that minimizes the surface of primitives required, and only expose the absolutely needed features.

Dynamic can be thought as a struct with a void* and a TypeRep*, which you can do some operations with TypeRep. All the extra machineary on top of primitives make it hard to describe a layout like this for haskell, because there is no such type directly implemented in the rts, but it's rather constructed in a higher level.

- https://www.seas.upenn.edu/~sweirich/papers/wadlerfest2016.pdf
- https://ecommons.cornell.edu/bitstream/handle/1813/5614/TR2003-1901.pdf?sequence=1&isAllowed=y

