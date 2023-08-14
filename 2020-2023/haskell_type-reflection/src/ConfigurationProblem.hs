{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module ConfigurationProblem where

import           Data.IORef
import           GHC.IO.Unsafe

-- functions like add, mul needs to be configured to work properly.
-- In this case, their behavior depends on the value of modulus.
--
-- Traditionally we pass configuration by
--  1. passing one to every single function
--  2. or we use a global variable
--  3. or we use dynamic variable.



-----------------------------------------------------------------------------
-- with explicit function passing
-----------------------------------------------------------------------------
newtype Modulus s a = Modulus a deriving (Eq, Show)
newtype M s a = M a deriving (Eq, Show)

add :: Integral a => Modulus s a -> M s a -> M s a -> M s a
add (Modulus m) (M a) (M b) = M $ (a + b) `mod` m

mul :: Integral a => Modulus s a -> M s a -> M s a -> M s a
mul (Modulus m) (M a) (M b) = M $ (a * b) `mod` m

unM :: M s a -> a
unM (M m) =  m


-- straight style
data AnyModulus a where
  AnyModulus :: Modulus s a -> AnyModulus a

makeModulus :: a -> AnyModulus a  -- exitential quantify it.
makeModulus a = AnyModulus (Modulus a)

test1 :: Integer
test1 = case makeModulus 4 of
          AnyModulus m ->
            let a = M 3
                b = M 5
             in unM $ add m (mul m a a) (mul m b b)


-- cps form. We don't even need to introduce a new exitential type wrapper.
-- just pass the threaded to a continuation and all computation is done
-- within.
withModulus :: a -> (forall s. Modulus s a -> w) -> w
withModulus m k = k (Modulus m)

test2 :: Integer
test2 = withModulus 4 $ \m ->
  let a = M 3
      b = M 4
   in unM $ add m (mul m a a) (mul m b b)


-----------------------------------------------------------------------------
-- with global mutable state.
-----------------------------------------------------------------------------

modref = unsafePerformIO (newIORef 0)
modval = unsafePerformIO (readIORef modref)

addG a b = (a + b) `mod` modval
subG a b = (a - b) `mod` modval
mulG a b = (a * b) `mod` modval

{- This really should be a simple thing but it's complicated in haskell..
-}
