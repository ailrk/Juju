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


module HLists where

import Data.Kind

-- a type level list. we need this because we can wrap a list
-- into a type with kind `Type`

infixr 5 `HCons`
data HList (as :: [Type]) :: Type where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

type family Head as where
  Head (HList '[]) = Nothing
  Head (HList (a ': as)) = Just a

type family FromJust (a :: Maybe b) :: b where
  FromJust (Just a) = a


typelist = True `HCons` 'a' `HCons` "hello" `HCons` HNil


-- note, because [] and [Type] are two different kinds, we can't
-- act on them uniformly, this is why we want to have a HList
-- wrapper on top.

