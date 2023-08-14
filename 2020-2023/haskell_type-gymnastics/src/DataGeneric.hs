
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module DataGeneric where

import           Data.Kind
import GHC.Natural


-- data generic programming is based on builiding an isomorphism
-- from normal adt to a datatype we can work on, so we can
-- performing operation on the mapped to data, then map it back.

newtype Leaf a = Leaf { getLeaf :: a }
class Generic a where
  type Rep a
  genericize :: a -> Rep a

data Foo = FooA (Either Int String) | FooB (Char, Bool) deriving Show

instance Generic Foo where
  type Rep Foo = Either (Either Int String) (Char, Bool)
  genericize (FooA x) = Left (x)
  genericize (FooB x) = Right (x)


-- then we can define functions act on different shapes.
-- in this example we count how many leaves are there in the adt.

class NumFields a where
  numFields :: a -> Natural

instance {-# OVERLAPPING #-} NumFields () where
  numFields _ = 0

instance NumFields a where
  numFields _ = 1

instance {-# OVERLAPPING #-} (NumFields a, NumFields b) => NumFields (a, b) where
  numFields (a, b) = numFields a + numFields b

instance {-# OVERLAPPING #-} (NumFields a, NumFields b) => NumFields (Either a b) where
  numFields (Left a) = numFields a
  numFields (Right b) = numFields b

getNumFields :: (Generic a, NumFields (Rep a)) => a -> Natural
getNumFields = numFields . genericize

test1 = let a = FooA (Left 1); b = FooA (Right "a");
            c = FooB ('a', False)
         in ( (a, getNumFields a), (b, getNumFields b)
            , (c, getNumFields c))
