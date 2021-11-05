{-# LANGUAGE RankNTypes #-}
module LinkedList where

import Data.IORef
import GHC.IO.Unsafe

-- still the same recursive definition, but now we go with a destructive
-- approach


nullptr :: forall a. IORef a
nullptr = unsafePerformIO $ newIORef (error "dangling ptr!")

data Node a = Node a (IORef (Node a))
newtype LinkedList a = LinkedList (IORef (Node a))

newNode :: a -> Node a
newNode a = Node a nullptr

-- O(1)
pushFrontNode :: a -> Node a -> IO ()
pushFrontNode x xs = do
  let (Node _ next) = newNode x
  writeIORef next xs

-- O(1)
popFrontNode :: Node a -> IO a
popFrontNode (Node a _) = return a

-- O(n)
pushBackNode :: a -> Node a -> IO ()
pushBackNode = undefined

-- O(n)
popBackNode :: Node a -> IO a
popBackNode = undefined

reverse :: Node a -> IO ()
reverse = undefined

mapList :: (a -> IO b) -> Node a -> IO ()
mapList f (Node x next) = do
  f x
  if next == nullptr
     then return ()
     else do
       rest <- readIORef next
       mapList f rest
