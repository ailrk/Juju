{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE NoImplicitPrelude #-}
module ANN where

import           Data.Kind
import           GHC.TypeLits


-- bootleg oracle
-- model trained by ANN is a machine that can answer decision problems
-- or function problems. If we think the model as a function, features
-- of data is the domain, label is the range.

-- ML models are estimation of real correlations. If we assume there exists a
-- real mapping between data and label that never fails, the model trained by
-- ANN give us a estimation of that function with O(1) complexity.

-- How do we model a neuro network?


-------------------------------------------------------------------------------
-- backpropagation


