module LambdaLifting where
import           Control.Monad.Trans.State as State

-- lambda lifting is similar to closure conversion that it eliminates free
-- variables in expressions by passing them as extra parameters.

-- It also lift all nested function definittion to the top level to simply
-- further compilation.

-- steps:
-- 0. rename each functions with unique names.
-- 1. convert free variable as extra parameter.
-- 2. moving functions to the top level.
-- 3. adjust all call sites accordingly.
--
-- For closures we need to defunctionalize first.
--
-- Lambda lifting is O(N^2), it's quite a expensive transformation.

type Var = String
data Expr = Var Var | Apply Expr [Expr] | Lambda [Expr] Expr

type UniqueId = Int
type LC a = State UniqueId a


someFunc :: IO ()
someFunc = putStrLn "someFunc"
