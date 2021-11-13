{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE StandaloneDeriving #-}
module TypeDirectedMemoization where

------------------------------------------------------------------------------
-- Type directed optimization
-- very interesting implementation for memoization
-- The idea is to define a table value for each data type, and recursively
-- build up table base on data family result.
-- Note GHC doesn't memoize functions, it just reuse forced values.

class Memo a where
  data Table a :: * -> *
  toTable :: (a -> w) -> Table a w
  fromTable :: Table a w -> (a -> w)

-- memoize boolean
instance Memo Bool where
  data Table Bool w = TBool w w deriving Show
  toTable f = TBool (f True) (f False)
  fromTable (TBool x y) b = if b then x else y

-- memoize sum types.
-- memoize both left  and right branches
instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f. Right))
  fromTable (TSum t _) (Left v) = fromTable t v
  fromTable (TSum _ t) (Right v) = fromTable t v

test1 = gb True
  where
    fb :: Bool -> Integer
    fb True = fromIntegral . length $ [gcd a b | a <- [1..100], b <- [1..100]]
    fb False = fromIntegral ((length [lcm a b | a <- [1..100], b <- [1..100]]) `div` 2)
    gb :: Bool -> Integer
    gb = fromTable (toTable fb)

test2 = let f' x = case x of { True -> Left True; False -> Right False }
         in  toTable f'

-- memoize tuple.
instance  (Memo a, Memo b) => Memo (a, b) where
  data Table (a, b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable $ \x -> toTable $ \y -> f (x, y))
  fromTable (TProduct t) (x, y) = fromTable (fromTable t x) y

test3 = toTable (\x -> case x of { True -> (True, True); False -> (False, False) })

-- memoize for recursive types
instance (Memo a) => Memo [a] where
  data Table [a] w = TList w (Table a (Table [a] w))
  toTable f = TList (f []) (toTable $ \x -> toTable $ \xs -> f (x:xs))
  fromTable (TList t _) [] = t
  fromTable (TList _ t) (x:xs) = fromTable (fromTable t x) xs

-- Table is generated lazily so it works for infinite list.

f' xs = case (xs :: [Bool]) of
         [] -> []
         (a:as) -> True : f' as
