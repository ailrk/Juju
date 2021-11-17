{-# LANGUAGE RankNTypes #-}

module Simple where

import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.Trans
import           Data.Foldable
import           Data.Traversable

import           Data.IORef


-- use binary function as infixed operator to make the data acted on obvious
infixBinop :: IO ()
infixBinop = do
  ref <- newIORef 0
  ref `writeIORef` 10


-- sequence a list of actions
fun2 :: IO ()
fun2 = do
  let xs = replicate 10 $ putStrLn "action"
  sequenceA_ xs


-- composing endo functors
foldFns :: Double -> Double
foldFns = foldr1 (.) [(* 10), (+ 1), (/ 100)]


-- fold klesli arrows
foldKlesli = foldr1 (<=<) [compute (+), compute subtract]
  where
    compute :: (Int -> Int -> Int) -> (Int -> IO Int)
    compute op n = do
      v <- readLn :: IO Int
      return $ op v n


-- lifting a function. For a function works on (m :: Type -> Type), we want to
-- lift it to get another function works on (t m).
--
-- technique: when returning a function, you can just define the
-- function itself that returns a final value.
--
-- When lifting, you want to resume some bindings exists in the context and add
-- dummy structures on top of them.
liftSomething :: (Monad m, Monad f)
              => ((x -> m a) -> x -> m b)
              -> ((x -> f (m a)) -> x -> f (m b))
liftSomething op f x = f x >>= \ ma -> return $ op (const ma) x



-- apply function to nested functor. just compose fmap and penetraing.
fmapfmap :: (Functor f, Monad m) => (a -> b) -> m (f a) -> m (f b)
fmapfmap f m = (fmap . fmap) f m


-- mapping a kelsli arrow into a functor.
-- (f =<<) is the monadic equivalence to (f $)
mapKlesli :: (Functor f, Monad m) => (a -> m b) -> f (m a) -> f (m b)
mapKlesli f m = fmap (f =<<) m


-- easily stack monad transformers
-- you can easily stuck monad transformers by applying the runner and
-- transformer runners api design are unfortunate, if you want to write in this
-- style you need to flip the runner.
--
-- The ratoinale might be a function takes `self` as the first parameter, but
-- really you want to make the biggest part the last so you can have long
-- definition.

gcdAndAct :: (Int -> IO ()) -> Int -> Int -> IO ()
gcdAndAct action m n = flip runReaderT action . flip evalStateT (m, n) $ do
  (m, n) <- get
  let n' = m `mod` n
  if n' == 1
     then liftIO $ action m
     else liftIO $ gcdAndAct action n n'
