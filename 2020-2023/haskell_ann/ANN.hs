{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeOperators #-}
module ANN where
import           Control.Monad.Random
import           Control.Monad.Trans.State
import           Data.Singletons
import qualified Data.Vector               as V
import           Numeric.LinearAlgebra     ((#>))
import qualified Numeric.LinearAlgebra     as M


-- input m, (m x n) * n + n
data Layer = L { baises :: !(M.Vector Double)     -- n
               , nodes  :: !(M.Matrix Double)     -- m x n
               }

data Network where
  Single :: !Layer -> Network
  (:+>) :: !Layer -> !Network -> Network

infixr 5 :+>

-- nonlinearize output
logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

runLayer :: Layer -> M.Vector Double -> M.Vector Double
runLayer (L b n) v = b + n #> v

runNetwork :: Network -> M.Vector Double -> M.Vector Double
runNetwork (Single l) !v = logistic (runLayer l v)
runNetwork (l :+> ns) !v = runNetwork ns (logistic (runLayer l v))

backProp :: Double -> M.Vector Double -> M.Vector Double -> Network -> Network
backProp rate input output = undefined
