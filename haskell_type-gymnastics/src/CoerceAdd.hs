{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE StandaloneDeriving #-}
module CoerceAdd where

class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance (Num a) => Add a a where
  type SumTy a a = a
  add a b = a + b

-- have to specialize this instance otherwise it will overlaps with other three
instance {-# OVERLAPPING #-} Add Double Double where
  type SumTy Double Double = Double
  add a b = a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add a Double where
  type SumTy a Double = Double
  add a b = fromIntegral a + b

instance {-# OVERLAPPING #-} (Integral a, Num a) => Add Double a where
  type SumTy Double a = Double
  add a b = a + fromIntegral b

-- ehh..
instance {-# OVERLAPPING #-} (Add Integer a) => Add Integer [a] where
  type SumTy Integer [a] = [SumTy Integer a]
  add a xs = fmap (add a) xs
