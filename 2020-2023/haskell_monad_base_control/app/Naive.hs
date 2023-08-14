module Naive where

import Control.Monad.State
import Control.Monad.Trans.Class

-- 1. we have a function IO a -> IO a

foo :: IO a -> IO a
foo action = do
  putStrLn "just print something"
  action

-- 2. and a value StateT s IO a
bar :: StateT Int IO Char
bar = StateT . const . return $ ('a', 0)

-- how do we apply foo on bar?

-- naive approach: mannually unwrap
foo' :: StateT s IO a -> StateT s IO a
foo' m = do
  s <- get
  (v, s') <- lift $ foo (runStateT m s)
  put s'
  return v


-- instead of using foo we use foo'
run1 :: IO ()
run1 = flip runStateT 0 (foo' bar) >> return ()


-- problem:
-- 1. need to write a wrapper mannually
-- 2. need to mannually unwrap
-- 3. this method is not composable in natural, every different type needs an
--    adhoc wrapper.
-- 4. When working with a monad transformer stack the amount of code increase
--    quickly.


------------------------------------------------------------------------------
-- really what we want is to `unlift` the (State Int) from the value bar instead
-- of lifting the parameter of foo into StateT monad.
-- But we can't do that naively by simply peeling off the StateT layer

unliftState :: StateT Int IO a -> IO a
unliftState m = fst <$> runStateT m 0


run2 :: IO ()
run2 = foo $ unliftState bar >> return ()


-- problem:
-- 1. state is completely forgot. after unliftState
-- 2. sometimes hard to write generic version. e.g here we need to pass a state
--    to peel of the StateT layer.

------------------------------------------------------------------------------
-- to solve the first problem of unliftState, we can save the state before
-- unlifting, get the value, and then put the value back into the original state
