{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module CyclicReference where

import           Data.Function
import           Data.Maybe

import Control.Monad.Fix
import Data.IORef
import Data.Foldable

-- It's common in imperative languages to have references refer to things
-- that doesn't exists yet, then initialize the thing in the next statement.
-- This is named value recursion, meaning the value is recursively used.
-- and we can achieve this by two ways:
--   1. MonadFix
--   2. RecursiveDo


-- imperative cyclic linked list
data Node1 = Node1 Int (IORef Node1)

-- a node points to itself.
selfReferencedNode = do go mknode; go mknodeRec
  where
    go mknode = do
      p <- mknode
      Node1 x q <- readIORef p; print x
      Node1 y _ <- readIORef q; print y
    mknode = mfix $ \p -> do
      p' <- newIORef (Node1 0 p)
      putStrLn "node created"
      return p'
    mknodeRec = do
      rec p <- newIORef (Node1 0 p)
      putStrLn "node created"
      return p

-- nodes point to each other.
cyclicReferencedNodes = do go mknode; go mknodeRec
  where
    go mknode = do
      (p, r) <- mknode
      Node1 x q <- readIORef p; print x
      Node1 y _ <- readIORef r; print y
    mknode = mfix $ \ ~(p, r) -> do
      p' <- newIORef (Node1 0 r)
      r' <- newIORef (Node1 1 p')
      putStrLn "nodes created"
      return (p', r')
    mknodeRec = do
      rec p <- newIORef (Node1 0 r)
          r <- newIORef (Node1 1 p)
      putStrLn "nodes created"
      return (p, r)

-- make a graph
--      0 -> 1 -> 4 -> 5
--           |
--           V
--           3 -> 2

data Node2 = Node2 Int ([IORef Node2])

simpleGraph = do go mknodes; go mknodesRec
  where
    go mknodes = do
      (n0, _, _, _, _, _) <- mknodes
      n <- readIORef n0
      visit n
    visit :: Node2 -> IO ()
    visit (Node2 x next) = do
      print x
      m <- traverse readIORef next
      traverse_ visit m
    mknodes = mfix $ \ ~(n0, n1, n2, n3, n4, n5) -> do
      n0' <- newIORef (Node2 0 [n1]); n1' <- newIORef (Node2 1 [n3, n4])
      n3' <- newIORef (Node2 3 [n2]); n2' <- newIORef (Node2 2 [])
      n4' <- newIORef (Node2 4 [n5]); n5' <- newIORef (Node2 5 [])
      putStrLn "graph created"
      return (n0', n1', n2', n3', n4', n5')
    mknodesRec = do
      rec n0 <- newIORef (Node2 0 [n1])
          n1 <- newIORef (Node2 1 [n3, n4])
          n3 <- newIORef (Node2 3 [n2])
          n2 <- newIORef (Node2 2 [])
          n4 <- newIORef (Node2 4 [n5])
          n5 <- newIORef (Node2 5 [])
      putStrLn "graph created"
      return (n0, n1, n2, n3, n4, n5)
    -- result : 0 1 3 2 4 5-
