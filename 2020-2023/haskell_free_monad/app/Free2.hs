{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE RankNTypes #-}
module Free2 where

import Control.Monad

-- Free algebra: free means the minial structure for satisfying the condition
-- of being an algebra
--
-- the structure is minially restricted by conditions that is relavent to the law.


--------------------------------- 1.
-- M is a free monoid over S that satisfy the minial condition for a monoid.
-- M only implies monoid law
-- : 1. associativity
-- : 2. left and right identity
-- and nothing else
data S

type M = [S]

-- addition on integer is not a free monoid
-- because addition on integer implies commutativity
--


--------------------------------- 2.
-- with the context above, a free monad is a monad that satisfy and only
-- satisfy minimal monadic laws.
--
-- for monad, we have
-- 1. (m >=> g) >=> h = m >=> (g >=> h)  associtivity
-- 2. return >=> m = m                   left identity
-- 3. m >=> return = m                   right identity
--
-- it's really a monoid




--------------------------------- 3.
-- endo functor
-- a monad is
-- 1. an endofunctor T : X -> X     (* -> *)
-- 2. has a nt mu: T x T -> T       (join :: m (m a) -> m a)
-- 3. has a nt nu: I -> T          (return :: a -> m a)
--
-- monoid law
-- 4. mu . T mu = mu . mu T
-- 5. mu . T nu = nu . mu T = 1


--------------------------------- 4.
-- define a monad without using Monad instance but only free
data Free f a
  = Pure a
  | Free (f (Free f a))

-- fmap to a free means map f all the way down
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free fa) = Free (fmap f <$> fa)


instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap


instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free fx >>= f = Free ((>>= f) <$> fx)

-- lift any functor value into Free monad
liftF :: Functor f => f a -> Free f a
liftF command = Free (fmap Pure command)


foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free as) = f as >>= foldFree f

------------------------------------------- note
-- 1. we only defines the command we need
-- 2. the command needs to be a functor
-- 3. use liftF to lift a command into Free to get a monad
-- 4 The functor embeded in Free is automatically a monand
newtype StateF s a = StateF { runStateF :: s -> (a, s) } deriving Functor


getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type State s = Free (StateF s)

get :: State s s
get = Free $ Pure <$> getF

put :: s -> State s ()
put s = Free $ Pure <$> putF s

-- at this point it's a monad


someComputation :: State Int ()
someComputation = do
  i <- get
  put $ i + 1
  pure ()



-- we need to interpret the Free monad
-- pure and >>= are moved to runState.
-- Free monad doesn't specify what does the monad means, so we need an
-- interpreter to define the semantics.
--
-- This also means we can have different interepreters for the saame
-- free monad.
runState :: State s a -> s -> (a, s)
runState (Pure x) s = (x, s)
runState (Free f) s =
  let (m, s') = runStateF f s
   in runState m s'


------------------------------------------- edsl

data ASTF t a
  = Add t t (t -> a)
  | Input (t -> a)
  | Output t a
  deriving Functor


type FreeAST t = Free (ASTF t)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add a b = liftF $ Add a b id

output :: t -> FreeAST t ()
output a = liftF $ Output a ()

program :: (Read a, Show a) => FreeAST a ()
program = do
  x <- input
  y <- input
  res <- add x y
  output res


runFreeAST :: FreeAST Int () -> IO ()
runFreeAST = foldFree interpFreeAST
  where
    interpFreeAST :: ASTF Int x -> IO x
    interpFreeAST (Add x y next) = pure $ next (x + y)
    interpFreeAST (Input next) = do
      val <- read <$> getLine
      return $ next val
    interpFreeAST (Output x next) = do
      putStrLn (show x)
      return $ next
