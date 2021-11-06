{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Loop where


-- combinator -> LoopConfig -> write interpret loop config.

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Data.Kind          (Type)

-- cases
-- from i to j        -> build [i, j..1]
-- from i to j step k -> build [i,j..k]
-- across xs          -> build [a0, a1, ..]
-- while              -> give [i, j..k], put in except monad, check
--                       before calling action, if unsatisfied then break.

newtype IndexList n = IndexList [n]

type Loop :: (Type -> Type) -> Type -> Type -> Type
data Loop m r a where
  From   :: Integral n
         => Loop m r a -> n
         -> Loop m r (n -> n -> IndexList n)

  To     :: Integral n
         => Loop m r (n -> n -> IndexList n) -> n
         -> Loop m r (n -> IndexList n)

  Step   :: Integral n
         => Loop m r (n -> IndexList n) -> n
         -> Loop m r (IndexList n)

  While  :: Loop m r [b] -> (b -> Bool) -> Loop m r ([b], b -> Bool)

  Across :: Traversable f => Loop m r a -> f a -> Loop m r (f a)

  Run    :: Loop m r a

  -- -- entry
  -- With   :: Loop m a a -> (a -> m r) -> Loop m a r

data SomeLoop where SomeLoop :: Loop m r a -> SomeLoop

from :: Integral n
     => Loop m r a -> n
     -> Loop m r (n -> n -> IndexList n)
from = From

to :: Integral n
   => Loop m r (n -> n -> IndexList n) -> n
   -> Loop m r (n -> IndexList n)
to = To

step = Step

across :: Traversable f => Loop m r a -> f a -> Loop m r (f a)
across = Across

while = While
loop = Run

evalLoop :: Monad m => Loop m r a -> m a
evalLoop Run = undefined

evalLoop (From loop i) = do
  _ <- evalLoop loop
  return $ \j s -> IndexList [i, j..s]
evalLoop (To loop j) = ($ j) <$> evalLoop loop
evalLoop (Step loop s) = ($ s) <$> evalLoop loop

evalLoop (Across loop xs) = do
  _ <- evalLoop loop
  return xs

evalLoop (While loop pred) = do
  xs <- evalLoop loop
  return (xs, pred)

-- entry
-- with takes a function, depends on what previous value it is it
-- pass the right funcion
--
-- pass a loop. step -> a -> m r, a takes [n]
--              to -> a -> m r,   a takes n -> [n]. we wrap another layer.

class With k conf | conf -> k where
  type LoopRet k
  with :: conf -> k -> LoopRet k


-- TODO makesure index list worksthe same as normal list.
instance (Monad m, Integral n)
      => With (IndexList n -> m r) (Loop m r (IndexList n)) where
  type LoopRet (IndexList n -> m r) = m r
  with loop@(Step _ _) k = do
    xs <- evalLoop loop
    k xs
  with _ _ = error "loop syntax error"

instance (Monad m, Integral n)
      => With (IndexList n -> m r) (Loop m r (n -> IndexList n)) where
  type LoopRet (IndexList n -> m r) = m r
  with loop@(To _ _) k = do
    f <- evalLoop loop
    k (f 1)
  with _ _ = error "loop syntax error"



-- -- how do you unrow this thing?
lang = (To (From Run 10) 20)
header = (Step (To (From Run 10) 20) 1)

-- -- evalLoop header should give (return [10 .. 20]), we can evalLoop
-- -- header >>= k


-- -- should give us (return . \x -> [10 .. n])
-- fromclause = (From Run 10)

-- start = Run

-- xs = loop `from` 10 `to` 20 `step` 1 `with` \i -> undefined
