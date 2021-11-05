{-# LANGUAGE DeriveFunctor #-}
module Pause where


import           Control.Monad
import           Control.Monad.Trans


data Pause m
 = Run (m (Pause m))    -- holds another pause with the same effect.
 | Done

pauseDemo1 :: Pause IO
pauseDemo1 = Run $ do
  putStrLn "begin"
  putStrLn $ "#1"
  return $ Run $ do
    putStrLn $ "#2"
    return $ Run $ do
      putStrLn "#3"
      return Done

-- how to run a pausable action?
-- a pasuable action can be separated into different steps.
runN :: Monad m => MonadFail m => Int -> Pause m -> m (Pause m)
runN 0 p = return p
runN n (Run m)
  | n < 0 = fail "n < 0!"
  | otherwise = do
      a <- m
      runN (n - 1) a
runN _ Done = return Done

runAll :: Monad m => Pause m -> m ()
runAll Done    = return ()
runAll (Run m) = m >>= runAll

data PauseT m r
  = RunT (m (PauseT m r))
  | DoneT r
  deriving Functor

instance Monad m => Applicative (PauseT m) where
  (<*>) = ap
  pure = return

instance Monad m => Monad (PauseT m) where
  return a = DoneT a
  DoneT r >>= f = f r
  RunT m >>= f  = RunT $ fmap (>>= f) m

instance MonadTrans PauseT where
  lift = RunT . (fmap DoneT)

pause :: Monad m => PauseT m ()
pause = undefined

joinP :: Monad m => PauseT m (PauseT m a) -> PauseT m a
joinP = undefined


main :: IO ()
main = undefined
