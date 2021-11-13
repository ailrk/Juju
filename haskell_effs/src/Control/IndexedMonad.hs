module Control.IndexedMonad where

-- https://bentnib.org/paramnotions-jfp.pdf
-- https://personal.cis.strath.ac.uk/conor.mcbride/Kleisli.pdf
-- https://kseo.github.io/posts/2017-01-12-indexed-monads.html

class IxMonad m where
  ireturn :: a -> m p p a
  ibind :: m p q a -> (a -> m q r b) -> m p r b

newtype MW m p q a = MW { unMW :: m a }

instance Monad m => IxMonad (MW m) where
  ireturn = MW . return
  ibind (MW m) f = MW (m >>= unMW . f)

newtype IxStateT m si so v = IxStateT { runIxStateT :: si -> m (so, v) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT $ \si -> return (si, x)
  ibind (IxStateT m) f = IxStateT $ \si -> m si >>= (\(sm, x) -> runIxStateT (f x) sm)

vsget :: Monad m => IxStateT m si si si
vsget = IxStateT $ \si -> return (si, si)

vsput :: Monad m => so -> IxStateT m si so ()
vsput so = IxStateT $ \_ -> return (so, ())
