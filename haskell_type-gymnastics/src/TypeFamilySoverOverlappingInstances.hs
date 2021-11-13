{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeFamilySoverOverlappingInstances where

import Data.Proxy

-- use singleton proxy to choose the right instance.
-- this approach still requires undecidable instance.

type family (F a) :: Bool where
  F Char = 'True
  F Bool = 'True
  F a = 'False

-- infer proxy from flag.

class ShowList a where
  showl :: [a] -> String

instance (F a ~ flag, ShowList' flag a) => ShowList a where
  showl = showl' (Proxy :: Proxy flag)

class ShowList' (flag :: Bool) a where
  showl' :: Proxy flag -> [a] -> String

instance ShowList' 'True Char where
  showl' _ x = x

instance ShowList' 'True Bool where
  showl' _ x = map toBinaryDigit x
    where
      toBinaryDigit False = '0'
      toBinaryDigit True = '1'

instance Show a => ShowList' 'False a where
  showl' _ x = show x
