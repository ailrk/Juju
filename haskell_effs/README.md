# Fun with effects and monads

Bullet points:

- Represent various effects in functional langauges.
- Compose klesli arrow to compose effects.
- Monad transformers as list of monads
- IO is lack of control flow.
- Peeling transformers in practice.
- Eq1, Ord1, Show1, Read1 and lifting instance out.
- mtl and tagless final
- Common class you can expect on transformers.
- Monad trans control and monad base control to unwrap and rewrap
- Indexed monad with initial and final state.
- Plucking constraints.
- Free Monad from functors.
- Algebraic effects with free monads and fusion.
- Writer monad performance
  - `++` and left nesting
  - using Endo to convert left nesting to right nesting.
  - Change a data structure (DList)
  - Lazy write and accumualted memory
- `liftCallCC`, `liftCallCC'` and StateT s m a
- Handy conversion functions between `MaybeT` and `ExceptT`
- Select monad for bracktracking algorithms
- Backward state monad and time traveling.
- Just go with `RWST r w s IO` and solve all headaches.
