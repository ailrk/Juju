{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE GADTs                #-}
module FinalTagless.FinalTaglessLC where

import           Data.Kind

-------------------------------------------------------------------------------
-- Taggless final HOAS interpreter for untyped lambda calclulus.
-------------------------------------------------------------------------------

class LC (repr :: Type -> Type) where
  con :: Int -> repr Int
  lambda :: (repr a -> repr b) -> repr (a -> b)
  apply :: repr (a -> b) -> repr a -> repr b

-- f algebra
data R a = R { unR :: a}

-- the implementation looks exactly the same as the eval for tagged iniital
-- HOAS.
instance LC R where
  con x = R x
  lambda f = R $ \x -> unR (f (R x))
  apply f a = R $ (unR f) (unR a)

run1 :: LC repr => repr Int
run1 = apply (lambda (\x -> x)) (con 10)


-------------------------------------------------------------------------------
-- Add boolean value and tuple,
-------------------------------------------------------------------------------

class LCExt1 (repr :: Type -> Type) where
  boolean :: Bool -> repr Bool
  tup :: repr a -> repr b -> repr (a, b)

instance LCExt1 R where
  boolean x = R x
  tup a b = R $ (unR a, unR b)


run2 :: (LC repr , LCExt1 repr) => repr (Bool, Int)
run2 = tup (apply (lambda (\x -> x)) (boolean True)) (con 2)


-------------------------------------------------------------------------------
-- Different interpreter, (pretty print)
-------------------------------------------------------------------------------

data E a where
  E :: forall a b . Show b => { unE :: b } -> E a

instance LC E where
  con n = E (show n)
  lambda f = E "lambda"
  apply f a = E "apply"
