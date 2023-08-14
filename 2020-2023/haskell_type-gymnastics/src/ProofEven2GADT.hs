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

module ProofEven2GADT where

import HLists

import Data.Kind

data Even as where
  EvenNil :: Even '[]
  EvenCons :: Even xs -> Even (a ': b ': xs)

-- using gadt to form a relation, serves a similar purpose as
-- using type famiy

data EvenPairs as bs where
  EvenPairsNil :: EvenPairs '[] '[]
  EvenPairsCons :: EvenPairs xs ys
                -> EvenPairs (a ': b ': xs) ((a, b) ': ys)

-- hereEvenPairs is the witness.

pairUp :: EvenPairs as bs -> HList as -> HList bs
pairUp EvenPairsNil HNil = HNil
pairUp (EvenPairsCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp even xs
