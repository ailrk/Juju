{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

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

module ProofEven3Constraint where

import HLists

import Data.Kind

-- isn't this the best solution?

data Even as where
  EvenNil :: Even '[]
  EvenCons :: Even xs -> Even (a ': b ': xs)

type family PairUp as where
  PairUp '[] = '[]
  PairUp (a ': b ': xs) = (a, b) ': PairUp xs

type family IsEven as :: Constraint where
  IsEven '[] = ()
  IsEven (_ ': _ ': xs) = IsEven xs


pairUp :: IsEven as => HList as -> HList (PairUp as)
pairUp HNil = HNil
pairUp (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp xs
