{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LinkedList where

import           Control.Monad
import           Data.Foldable
import           Data.IORef
import           GHC.IO.Unsafe

-- still the same recursive definition, but now we go with a destructive
-- approach


nullptr :: forall a. IORef a
nullptr = unsafePerformIO $ newIORef (error "dangling ptr!")

setnull ref = join (writeIORef ref <$> readIORef nullptr)

data Node a = Node a (IORef (Node a)) | Nil
newtype LinkedList a = LinkedList (IORef (Node a))

instance Show a => Show (Node a) where
  show (Node a ref) = show "<Node " ++ show a ++ ">"
  show Nil = "Nil"
  {-# INLINE show #-}

newNode :: a -> IO (Node a)
newNode x = do
  emptyRef <- newIORef Nil
  return (Node x emptyRef)
{-# INLINE newNode #-}

empty :: IO (LinkedList a)
empty = LinkedList <$> newIORef Nil
{-# INLINE empty #-}

pushFront :: a -> LinkedList a -> IO ()
pushFront x (LinkedList ref) = do
     oldHead <- readIORef ref
     newHead <- Node x <$> newIORef oldHead
     ref `writeIORef` newHead
{-# INLINE pushFront #-}

-- O(1)
popFront :: LinkedList a -> IO (Maybe a)
popFront (LinkedList ref) = do
  node <- readIORef ref
  case node of
    Nil -> return Nothing
    Node a restRef -> do
      rest <- readIORef restRef
      writeIORef ref rest
      return (Just a)
{-# INLINE popFront #-}

-- O(n)
pushBack :: a -> LinkedList a -> IO ()
pushBack x (LinkedList ref) = do
  node <- readIORef ref
  push ref x node
  where
    push prevRef x Nil = join $ (prevRef `writeIORef`) <$> newNode x
    push prevRef x (Node _ nRef) = do
      n <- readIORef nRef
      case n of
        Nil -> join $ (nRef `writeIORef`) <$> newNode x
        Node _ nnRef -> do
          nn <- readIORef nnRef
          push nnRef x nn
{-# INLINE pushBack #-}

-- O(n)
popBack :: forall a . LinkedList a -> IO (Maybe a)
popBack (LinkedList ref) = do
  node <- readIORef ref
  pop node
  where
    pop :: Node a -> IO (Maybe a)
    pop Nil = return Nothing
    pop (Node x nRef) = do
      next <- readIORef nRef
      ref `writeIORef` next
      return (Just x)
{-# INLINE popBack #-}

mapNodeList :: (Node a -> IO b) -> LinkedList a -> IO ()
mapNodeList f (LinkedList ref) = do
  if ref == nullptr
     then return ()
     else do
       n <- readIORef ref
       case n of
        Node x rest -> do f n; mapNodeList f (LinkedList rest)
        Nil         -> return ()

mapList_ :: (a -> IO b) -> LinkedList a -> IO ()
mapList_ f (LinkedList ref) = do
  if ref == nullptr
     then return ()
     else do
       (Node x rest) <- readIORef ref
       f x
       mapList_ f (LinkedList rest)

fromList :: [a] -> IO (LinkedList a)
fromList xs = do
  list <- empty
  sequence_ [ pushFront n list | n <- xs]
  return list

xsio :: IO (LinkedList Int)
xsio = do
  list <- empty
  pushFront 1 list
  pushFront 3 list
  pushBack  9 list
  pushBack  8 list
  return list
