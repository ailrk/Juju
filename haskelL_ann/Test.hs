module Test where

import Control.Monad.Random
import ANN
import Numeric.LinearAlgebra

randomLayers :: MonadRandom m => Int -> Int -> m Layer
randomLayers i o = do
  seed1 <- getRandom
  seed2 <- getRandom
  let b = randomVector seed1 Uniform o * 2 - 1
  let n = uniformSample seed2 o (replicate i (-1, 1))
  return $ L b n

randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o = Single <$> randomLayers i o
randomNet i (h:hs) o = (:+>) <$> randomLayers i h <*> randomNet h hs o
