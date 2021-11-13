{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Fun with typeclass
-- https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/typefun.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fassoc-types%2Ffun-with-type-funs%2Ftypefun.pdf
module Types.TypeFam2 where

o------------------------------------------------------
-- without type family we need to define the same typeclass with multiparameter
-- typeclass and functional dependency

class Mutation' m r | m -> r where
  newRef' :: a -> m (r a)
  readRef' :: (r a) -> m a
  writeRef' :: (r a) -> a -> m ()

-- this works.
instance Mutation' IO IORef where
  newRef' = newIORef
  readRef' = readIORef
  writeRef' = writeIORef

instance Mutation' (ST s) (STRef s) where
  newRef' = newSTRef
  readRef' = readSTRef
  writeRef' = writeSTRef

------------------------------------------------------------------------------
-- Multi paramter type class + type family for implicit conversion
-- say we want add works for int and double. We need int to double essentially.

-- Type relation is a bit tricky when you introduce lots of bounded
-- type variables. Too many overlapping instances!

class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance (Num a) => Add a a where
  type SumTy a a = a
  add a b = a + b

-- have to specialize this instance otherwise it will overlaps with other
-- three.
instance {-# OVERLAPPING #-} Add Double Double where
  type SumTy Double Double = Double
  add a b = a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add a Double where
  type SumTy a Double = Double
  add a b = fromIntegral a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add Double a where
  type SumTy Double a = Double
  add a b = a + fromIntegral b

-- ehh..
instance {-# OVERLAPPING #-} (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add a xs = fmap (add a) xs


-- >>> add (1 :: Int) (2 :: Int)
-- >>> add (1.4 :: Double) (2 :: Integer)
-- >>> add (1.4 :: Double) (2 :: Int)
-- >>> add (1 :: Int) (2.5 :: Double)
-- >>> add (1.3 :: Double) (2.5 :: Double)
-- 3
-- 3.4
-- 3.4
-- 3.5
-- 3.8

------------------------------------------------------------------------------
-- Graph with data family
-- data family allows you to define data type associate to Graph.

class Graph g where
  type Vertex g
  data Edge g
  src :: Edge g -> Vertex g
  target :: Edge g -> Vertex g
  outEdges :: g -> Vertex g -> [Edge g]

-- Note, type introduced by `data` is injective.
-- that is, f a ~ f b <=> a ~ b
-- this property needs to be specified by type family.

-- e.g with type family, this is clearly not injective.
type family G2 n where
  G2 Int = Int
  G2 Bool = Char

-- on the other hand, a type introduced by data keyword is always injective.
data G3 a = G3 a
type GTy = G3 Int

newtype G1 = G1 [Int]
instance Graph G1 where
  type Vertex G1 = Int
  data Edge G1 = Edge1 (Maybe Int)  -- the data type associates with the instance.
  src = undefined     -- whatever
  target = undefined
  outEdges = undefined

------------------------------------------------------------------------------
-- Type directed optimization

----------
-- type directed memoization
--
-- very interesting implementation for memoization
-- The idea is to define a table value for each data type, and recursively
-- build up table base on data family result.
--
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

-- the function will not memoize by itself.
fb :: Bool -> Integer
fb True = fromIntegral . length $ [gcd a b | a <- [1..100], b <- [1..100]]
fb False = fromIntegral ((length [lcm a b | a <- [1..100], b <- [1..100]]) `div` 2)

gb :: Bool -> Integer
gb = fromTable (toTable fb)

-- toTable for Bool will evaluate f for all values in the domain.
-- >>> toTable not
-- TBool False True
-- >>> toTable fb
-- TBool 10000 5000

-- Then we can memoize by exploiting GHC's value reuse feature

-- >>> fb True + fb True
-- 20000
-- >>> gb True + gb True
-- 20000
-- >>> gb True + gb False
-- 15000

-- This technique can be generalized of course.

-- memoize sum types.
-- memoize both left  and right branches
instance (Memo a, Memo b) => Memo (Either a b) where
  data Table (Either a b) w = TSum (Table a w) (Table b w)
  toTable f = TSum (toTable (f . Left)) (toTable (f. Right))
  fromTable (TSum t _) (Left v) = fromTable t v
  fromTable (TSum _ t) (Right v) = fromTable t v

-- we also uses the same stragety apply for all arguments for the first
-- call.
-- >>> let f' x = case x of { True -> Left True; False -> Right False }
-- >>> toTable f'
-- TBool (Left True) (Right False)

-- >>> let f' = (\x -> case x of { Left True -> True; Right False -> False })
-- >>> (fromTable . toTable)  f' (Left True)
-- True

-- memoize tuple.
instance  (Memo a, Memo b) => Memo (a, b) where
  data Table (a, b) w = TProduct (Table a (Table b w))
  toTable f = TProduct (toTable $ \x -> toTable $ \y -> f (x, y))
  fromTable (TProduct t) (x, y) = fromTable (fromTable t x) y

-- >>> toTable (\x -> case x of { True -> (True, True); False -> (False, False) })
-- TBool (True,True) (False,False)

-- >>> (\(TProduct x) -> x) $ toTable (\(x, y) -> (y :: Bool, x :: Bool))
-- TBool (TBool (True,True) (False,True)) (TBool (True,False) (False,False))

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


-- >>> toTable f' (repeat True)
-- Couldn't match expected type ‘[Bool] -> t’
--             with actual type ‘Table [Bool] [Bool]’


-- With basic inductive types, sum and product type, we can create an
-- isomorphism between real data types and memoized version.

------------------------------------------------------ ------------------------

