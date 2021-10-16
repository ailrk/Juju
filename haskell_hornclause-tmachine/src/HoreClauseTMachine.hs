{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HoreClauseTMachine where

import           Data.Kind
import           GHC.TypeLits

type HList :: [Type] ->  Type
data HList n

class Reverse xs ys
instance Reverse' xs (HList '[]) ys ys => Reverse xs ys

class Reverse' xs rs ys out
instance Reverse' (HList '[]) ys ys (HList '[])
instance Reverse' xs (HList (x ': rs)) ys bound
      => Reverse' (HList (x ': xs))  rs ys (b_ ': bound)


