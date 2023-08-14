module STM where

-- shared memory approach with transaction.

-- Pros:
--  - Much simpler then locked based shared memory model.
--  - Avoid deadlock
--  - Don't need to restructure the code.
--
-- Cons:
--  - IO operations are hard to rollback.



import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S

-- stm is about composing atomic operations into a bigger atomic operation.
-- with stm we don't need to worry about the ordering of take and put of
-- tvars within a stm block, because the whole block is guaranteed to execute
-- atomically, the process is protected from other thread.


data Desktop = Desktop deriving (Eq, Ord)
data Window = Window deriving (Eq, Ord)

type Display = (M.Map Desktop (TVar (S.Set Window)))



moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (S.delete win wa)
  writeTVar mb (S.insert win wb)
  where
    ma = disp M.! a
    mb = disp M.! b



swapWindow :: Display -> Window -> Desktop -> Window -> Desktop -> IO ()
swapWindow disp w a v b = atomically $ do
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a


-- why stm is a separate monad outside of IO:
--   STM allows us to rollback a transaction, but to roll back a transaction we
--   need to keep track of the effect the transaction has taken.
--   IO allows aribitrary side effects, and some of them can not be easily
--   undone. (e.g make true random number from /dev/random etc. )
--
--   To achive the roll back functionality, we need to restirct the amount of
--   side effects we are allowed to perform. In the case of STM, we only
--   allow mutation over TVar.


-- blocking with retry


newtype TMVar' a = TMVar' (TVar (Maybe a))

newEmptyTMVar' :: STM (TMVar' a)
newEmptyTMVar' = do
  t <- newTVar Nothing
  return (TMVar' t)


-- semantic of retry is to retry the stm block until the desired condition is met
-- when calling retry, the current thread is blocked,
takeTMVar' :: TMVar' a -> STM a
takeTMVar' (TMVar' t) = do
  m <- readTVar t
  case m of
    Nothing -> retry  -- retry the whole STM block
    Just a -> do
      writeTVar t Nothing
      return a


putTMVar' :: TMVar' a -> a -> STM ()
putTMVar' (TMVar' t) a = do
  m <- readTVar t
  case m of
    Nothing -> do
      writeTVar t (Just a) >> return ()
    Just _ -> retry



-- blocking until something changes

render :: S.Set Window -> IO ()
render = undefined


type UserFocus = TVar Desktop

getWindows :: Display -> UserFocus -> STM (S.Set Window)
getWindows disp focus = do
  desktop <- readTVar focus
  readTVar (disp M.! desktop)


-- render windows if user focus changes
-- fetching windows of the deskopt user is focusing on.
-- if there is change to windows, we redender, otherwise we just wait.
-- retry has very low overhead, it's ok to use it as a communication mechanism
renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus
  loop wins
  where
    loop wins = do
      render wins
      next <- atomically $ do
        wins' <- getWindows disp focus
        if (wins == wins')
           then retry
           else return wins'
      loop next


takeEitherTMVar' :: TMVar a -> TMVar b ->  STM (Either a b)
takeEitherTMVar' ma mb =
  fmap Left (takeTMVar ma) `orElse` fmap Right (takeTMVar mb)


-- channel with STM

data TChan' a = TChan' (TVar (TVarList a)) (TVar (TVarList a))

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)



newTChan' :: STM (TChan' a)
newTChan' = do
  hole <- newTVar TNil
  readEnd <- newTVar hole
  writeEnd <- newTVar hole
  return (TChan' readEnd writeEnd)


readTChan' :: TChan' a -> STM a
readTChan' (TChan' readVar _) = do
  listHead <- readTVar readVar
  head <- readTVar listHead
  case head of
    TNil -> retry
    TCons val rest -> do
      writeTVar readVar rest
      return val


writeChan' :: TChan' a -> a -> STM ()
writeChan' (TChan' _ writeVar) a = do
  newListEnd <- newTVar TNil
  listEnd <- readTVar writeVar
  writeTVar writeVar newListEnd
  writeTVar listEnd (TCons a newListEnd)


-- limitation of stm
--
--   fairness. in STM a thread can block on arbitrary condition, so we can't
--   guarantee the order of thhings, becuase at any given moment another
--   thread can be woken by certain condition
--
--   with mvar only apporach a blocked thread can only be waken by a putMVar,
--   which can be precisely controlled.

-- server


