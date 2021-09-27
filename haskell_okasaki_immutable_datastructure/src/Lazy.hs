{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lazy where

import           Control.Monad

-------------------------------------------------------------------------------
-- Represent a suspend computation.
-- wrap wrap
data Suspend a = Suspend { unSuspend :: !(() -> a) }

instance Functor Suspend where
  fmap f (Suspend a) = Suspend $ f . a

-- effect here is just keep the value in the dummy lambda.
instance Applicative Suspend where
  pure a =  delay $ \_ -> a
  Suspend f <*> Suspend m = Suspend (f <*> m)

instance Monad Suspend where
  return = pure
  m >>= f = f (force m)

instance Show a => Show (Suspend a) where
  show _ = "<a suspend operation>"

delay :: (() -> a) -> Suspend a
delay = Suspend

force :: Suspend a -> a
force !s = (unSuspend s) ()

-------------------------------------------------------------------------------
strictAdd :: Num a => a -> a -> a
strictAdd !a !b = a + b

-- >>> force . delay $ (\_ -> strictAdd 1 2)
-- 3

-------------------------------------------------------------------------------
-- A lazy stream
-- Because we are trying to simulate lazy evaluation, all function paramters
-- should be strict.
-- Note the overhead for the suspension on every StreamCell
data StreamCell a = Nil | Cons !(a, Stream a) deriving Show
data Stream a = Stream !(Suspend (StreamCell a)) deriving Show

instance Semigroup (StreamCell a) where
 Nil <> c = c
 c <> Nil = c
 Cons (a, as) <> rest
    | bs@(Cons _) <- headStream as = Cons (a, consStream (bs <> rest))
    | Nil <- headStream as = Cons (a, consStream rest)

instance Monoid (StreamCell a) where
  mempty = Nil

instance Functor StreamCell where
  fmap !f !Nil = Nil
  fmap !f !(Cons (a, rest)) = let h = headStream rest
                             in Cons (f a, Stream . pure $ fmap f h)

-- instances for streams
instance Semigroup (Stream a) where
  Stream xs <> Stream ys = consStream (force xs <> force ys)

instance Monoid (Stream a) where
  mempty = emptyStream

instance Applicative  StreamCell where
  pure a = Cons (a, emptyStream)
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  Cons (f, xs) <*> Cons (v, ys) = Cons (f v, consStream $ (headStream xs) <*> (headStream ys))

instance Functor Stream where
  fmap !f !xs = consStream (fmap f (headStream xs))

-- >>> toList $ fmap (+1) (fromList [1, 2, 3])
-- [2,3,4]

instance Applicative Stream where
  pure = consStream . pure
  a <*> b = consStream $ headStream a <*> headStream b

consStream :: StreamCell a -> Stream a
consStream !c = Stream . pure $ c

emptyStream :: Stream a
emptyStream = Stream . pure $ Nil

fromList :: [a] -> Stream a
fromList ![]     = Stream (pure Nil)
fromList !(x:xs) = Stream (pure $ Cons (x, fromList xs))

toList :: Stream a -> [a]
toList !(Stream xs)
  | Nil <- force xs = []
  | Cons (!a, xs) <- force xs = a : toList xs

-- >>> fromList [1, 2, 3]
-- Stream <a suspend operation>


headStream :: Stream a -> StreamCell a
headStream !(Stream xs)
  | c@(Cons (_, _)) <- force xs = c
  | Nil <- force xs = Nil

tailStream :: Stream a -> Stream a
tailStream (Stream xs)
  | (Cons (_, rest)) <- force xs = rest
  | Nil <- force xs = emptyStream

-- >>> headStream . fromList $ [1, 2]
-- >>> headStream . fromList $ ([] :: [Int])
-- Just 1
-- Nothing

-------------------------------------------------------------------------------
-- Monolithc: drop, reverse
-- Incremental: take, <>

-- Force the pattern when we apply take

-- take is incremental. Each time we take a new element we create a new
-- suspend operation.
takeStream :: Int -> Stream a -> Stream a
takeStream !n !(Stream xs)
  | (Cons (a, _)) <- force xs, n <= 1 = consStream (Cons (a, mempty))
  | (Cons (a, rest)) <- force xs =  consStream (Cons (a, (takeStream (n - 1) rest)))
  | Nil <- force xs = Stream xs

-- >>> toList . takeStream 3 . fromList $ [1, 2, 3, 4, 5]
-- >>> toList . takeStream 1 . fromList $ [1, 2, 3, 4, 5]
-- [1,2,3]
-- [1]

-- drop is monolithic, once we start to drop the first element, we need to
-- recursively all the entire function until it ends.
dropStream :: Int -> Stream a -> Stream a
dropStream !n !(Stream xs)
  | (Cons (a, as)) <- force xs, n <= 1 = as
  | (Cons (a, as)) <- force xs = dropStream (n - 1) as
  | Nil <- force xs = Stream xs

-- >>> toList . dropStream 3 . fromList $ [1, 2, 3, 4, 5]
-- >>> toList . dropStream 3 . fromList $ [] :: [Int]
-- [4,5]
-- []


-- reverse is a monolithic operation, the recursive call to reverseStream is
-- never delayed.
reverseStream :: Stream a -> Stream a
reverseStream (Stream xs)
  | Nil <- force xs = mempty
  | (Cons (a, as)) <- force xs = reverseStream as <> pure a
  | otherwise = undefined


-- >>> toList . reverseStream . fromList $ [1, 2, 3, 4, 5]
-- [5,4,3,2,1]

----------------------
-- If a type only supports monolithic operation, it shouldn not be strict in
-- the first place.
