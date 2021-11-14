{-# LANGUAGE LambdaCase #-}
module Stream where

-- handling streaming data.
-- In haskell, you don't write though writer monoad like you don't stream with
-- lazy list. It seems all the benefits haskell has are actually not benefit.

import Control.Monad
import Control.Monad.Trans.Class


-- coroutine based generator. the klesli arrow here is actuall the continuation
-- of the suspended computation.

newtype Generator a m x
  = Generator { bounceGen :: m (Either (a, Generator a m x) x) }

instance Monad m => Functor (Generator a m) where
  fmap f m = m >>= return . f

instance Monad m => Applicative (Generator a m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Generator a m) where
  return = Generator . return . Right
  m >>= k = Generator $ bounceGen m >>= \case
    Left (a, cont) -> return $ Left (a, cont >>= k)
    Right x -> bounceGen (k x)

instance MonadTrans (Generator a) where
  lift = Generator . fmap Right

yield :: Monad m => a -> Generator a m ()
yield a = Generator $ return (Left (a, return ()))

-- unfold a generator until it finishes.
runGenerator :: Monad m => Generator a m x -> m ([a], x)
runGenerator = run id
  where
    run f (Generator g) =
      g >>= \case
        Left (a, cont) -> run (f . (a:)) cont
        Right x -> return (f [], x)

triple :: Monad m => a -> Generator a m ()
triple x = do yield x; yield x; yield x

loop :: Generator String IO ()
loop = do
  str <- lift getLine
  when (str /= "") $  do
    yield str
    loop

alternate :: Monad m => Generator a m () -> Generator a m () -> Generator a m ()
alternate g1 g2 = Generator $ liftM2 go (bounceGen g1) (bounceGen g2)
  where
    go (Left (a, cont)) (Left (b, cont')) =
      Left (a, Generator $ return $ Left (b, alternate cont cont'))
    go (Left (a, cont)) (Right _)  = Left (a, cont)
    go (Right _) (Left (b, cont')) = Left (b, cont')
    go (Right _) (Right _)         = Right ()


test1 :: IO ([Int], ())
test1 = runGenerator $ alternate (triple 2) (triple 3)
