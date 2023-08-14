module Pipe where


data Pipe a b r
  = Pure r
  | Await (a -> Pipe a b r)
  | Yield b (Pipe a b r)


(<-<) :: Pipe b c r -> Pipe a b r -> Pipe a c r
Pure r     <-< _         = Pure r
Yield b p1 <-< p2        = Yield b (p1 <-< p2)
Await f    <-< Yield b p = f b <-< p
p          <-< Await f   = Await $ \a -> p <-< f a
_          <-< Pure r    = Pure r


(>->) :: Pipe a b r -> Pipe b c r -> Pipe a c r
(>->) = flip (<-<)

cat :: Pipe a a r
cat = Await $ \a -> Yield a cat
