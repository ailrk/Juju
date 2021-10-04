module Defunctoinalization where

import           Control.Monad.Trans.State as State
type Var = String
data Expr = Var Var | Apply Expr [Expr] | Lambda [Expr] Expr

type UniqueId = Int
type LC a = State UniqueId a
