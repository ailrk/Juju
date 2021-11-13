{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module SimpleWithness where

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


import HLists

import Data.Kind

-- sum a list that has lenght up to 3.
-- this type is merely a witness that proves the property holds.

data OneToThree as where
  One :: OneToThree '[Int]
  Two :: OneToThree '[Int, Int]
  Three :: OneToThree '[Int, Int, Int]

sumUpToThree :: OneToThree as -> HList as -> Int
sumUpToThree One (x `HCons` HNil) = x
sumUpToThree Two (x `HCons` y `HCons` HNil) = x + y
sumUpToThree Three (x `HCons` y `HCons` z `HCons` HNil) = x + y + z

test1 = sumUpToThree Two (3 `HCons` 2 `HCons` HNil)
test2 = sumUpToThree One (2 `HCons` HNil)
