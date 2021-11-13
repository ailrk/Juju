{-# LANGUAGE LambdaCase #-}
module Stream where

-- handling streaming data.
-- In haskell, you don't write though writer monoad like you don't stream with
-- lazy list. It seems all the benefits haskell has are actually not benefit.

import Control.Monad
import Control.Monad.Trans.Class

-- either a suspension or the final result.
newtype Generator a m x =
  Generator { bounceGen :: m (Either (a, Generator a m x) x)}

instance Monad m => Functor (Generator a m) where
  fmap f (Generator g) = Generator $ do
    x <- g
    fmap f x

