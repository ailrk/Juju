{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Dyn where

import           Data.Kind       (Type)
import           Type.Reflection

data Dynamic where Dynamic :: TypeRep a -> a -> Dynamic

toDyn :: Typeable a => a -> Dynamic
toDyn a = Dynamic (typeOf a) a

fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn (Dynamic trep x) def
  | Just HRefl <- trep `eqTypeRep` (typeOf def) = x
  | otherwise = def

fromDynamic :: forall a . Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic trep x)
  | Just HRefl <- let rep = typeRep :: TypeRep a in trep `eqTypeRep` rep = Just x
  | otherwise = Nothing

dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dynamic (Fun argrep1 retrep) f) (Dynamic argrep2 x)
  | Just HRefl <- argrep1 `eqTypeRep` argrep2
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind retrep
  = return $ Dynamic retrep (f x)
dynApply _ _ = Nothing

dynApp :: Dynamic -> Dynamic -> Dynamic
dynApp f x =
  case dynApply f x of
    Just d -> d
    otherwise -> errorWithoutStackTrace ("wrong dynamic application")
