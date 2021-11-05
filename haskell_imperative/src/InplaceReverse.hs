{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module InplaceReverse where


import           Data.StateVar

import           Foreign.Storable
import Foreign
import Control.Monad.IO.Class
import Control.Monad
import Data.Foldable


-- statevar is a simple wrapper of mutable values.
-- the only restriction is to use them in MonadIO.

makeStateVarFromPtr :: Storable a => Ptr a -> StateVar a
makeStateVarFromPtr p = makeStateVar (peek p) (poke p)

declare :: forall a . Storable a => IO (StateVar a)
declare = makeStateVarFromPtr <$> malloc

declares :: forall a. Storable a => Int -> IO [StateVar a]
declares n = sequence $ replicate n declare

swapStateVar :: StateVar a -> StateVar a -> IO ()
swapStateVar x y = do
  xv <- get x
  yv <- get y
  x $= yv
  y $= xv

inplaceReverse :: [StateVar a] -> IO [StateVar a]
inplaceReverse xs = do
  let len = length xs
  [i, j] <- declares @Int 2
  i $= 0
  j $= len - 1
  loop i j xs
  return xs
  where
    loop i j xs = do
      { iv <- get i;
        jv <- get j;
        if iv == jv || iv + 1 > jv
         then return ()
         else do
           { swapStateVar (xs !! iv) (xs !! jv);
             i $~ (+1);
             j $~ (subtract 1);
             loop i j xs;
           }
      }

printXs :: Show a => [StateVar a] -> IO ()
printXs = traverse_ (print <=< get)

reverseIt = do
  xs <- do
    vars <- declares @Int 10
    traverse (\(a, i) -> a $= i) (zip vars [1..])
    return vars
  putStrLn "orignal list"
  printXs xs
  inplaceReverse xs
  putStrLn "reversed list"
  printXs xs
