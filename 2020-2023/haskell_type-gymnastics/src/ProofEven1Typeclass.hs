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

module ProofEven1Typeclass where

import HLists

import Data.Kind

-- you can read backwards. if you can provide EvenCons, then
-- (a ': b' : xs) must exists. Or rather we are pattern mathcing
-- on a (a ': b' : xs) really.
data Even as where
  EvenNil :: Even '[]
  EvenCons :: Even xs -> Even (a ': b ': xs)

type family PairUp as where
  PairUp '[] = '[]
  PairUp (a ': b ': xs) = (a, b) ': PairUp xs

class IsEven as where evenProof :: Even as
instance IsEven '[] where evenProof = EvenNil
instance IsEven as => IsEven (a ': b': as) where
  evenProof = EvenCons evenProof

pairUp :: IsEven as => HList as -> HList (PairUp as)
pairUp = go evenProof
  where
    go :: Even as -> HList as -> HList (PairUp as)
    go EvenNil HNil = HNil
    go (EvenCons even) (x `HCons` y `HCons` xs) =
      (x, y) `HCons` go even xs

test1 = pairUp (() `HCons` "hi" `HCons` 1 `HCons` () `HCons` HNil)
