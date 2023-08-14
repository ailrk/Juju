{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module PartitionedCPS where

-- Naive cps converter
import           Control.Monad.Trans.State
import           Data.Char                    (isDigit, isLetter, isSpace)
import qualified Text.ParserCombinators.ReadP as P
