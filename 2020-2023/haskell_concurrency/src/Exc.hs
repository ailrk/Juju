{-# LANGUAGE ScopedTypeVariables #-}
module Exc where

import Control.Concurrent
import Control.Exception
import System.CPUTime
import Text.Printf
import Network.HTTP.Client
import Network.HTTP.Types.Status
import qualified Data.ByteString.Lazy as BLS
import Data.Foldable (traverse_)
import System.IO
import Control.Monad
import System.Random
import Data.Unique


-- cancelation, timeout, and exceptions on haskell threads.

-- how to design cancelation of a thread?
-- 1. the victim keep poling on some condition. Once the condition is true the
--    victim kill itself.
-- 2. the victim is immesiately killed in some way.


-- haskell suports asynchronous exception to force signalling a kill to victirms.
-- it's hard to use this mechanism in an imperative langauge, because force
-- killing a thread means leave things in an undefined state.




data Async a = Async ThreadId (MVar (Either SomeException a))

async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyMVar
  t <- forkIO (try action >>= putMVar var)
  return (Async t var)

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async t var) = readMVar var

wait :: Async a -> IO a
wait a = do
  r <- waitCatch a
  case r of
    Left e -> throwIO e
    Right a -> return a

cancel :: Async a -> IO ()
cancel (Async t var) = throwTo t ThreadKilled




-------------------------------------------------------------------------------
-- throw async exception

sites = ["http://www.google.com",
         "http://example.com/",
         "http://example.net/",
         "http://example.edu/"
        ]


timeIt :: IO a -> IO (Double, a)
timeIt ioa = do
  t1 <- getCPUTime
  a <- ioa
  t2 <- getCPUTime
  let t :: Double
      t = fromIntegral (t2 - t1) * 1e-12
  return (t, a)


getUrl :: Manager -> String -> IO (BLS.ByteString)
getUrl manager url = do
  request <- parseRequest url
  response <- httpLbs request manager
  return (responseBody response)


timeDownload :: Manager -> String -> IO ()
timeDownload manager url = do
  (time, page) <- timeIt $ getUrl manager url
  n <- randomRIO (1, 500000)
  threadDelay n
  printf "downloaded %s (%d bytes, %.2fs)\n" url (BLS.length page) time

run :: IO ()
run = do
  manager <- newManager defaultManagerSettings
  as <- traverse (async . timeDownload manager) (replicate 100 "http://example.com/")

  forkIO $ do
    hSetBuffering stdin NoBuffering
    forever $ do
      putStr "> "
      c <- getChar
      putChar '\n'
      when (c == 'q') $ do
        traverse_ cancel as
        pid <- myThreadId
        killThread pid
  traverse_ wait as




-------------------------------------------------------------------------------
-- protect from inconsistent state caused by async exception
-- principle: store state


-- the entire block is masked, async exception can only be raied in (f a)
-- masked region disable async exceptions, restore recover the delivery, but
-- the recovered region is explicit.
-- In anothre word, in mask async exception behaves like a synchronous exception
problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = mask $ \restore -> do
  a <- takeMVar m
  r <- restore (f a) `catch` \(e :: SomeException) -> do putMVar m a; throw e
  putMVar m r


-- we want to mask the whole bracket block, only allows async exception
-- happen on (action a)

bracket' :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket' before after action =
  mask $ \restore -> do
    a <- before
    r <- restore (action a) `onException` after a
    _ <- after a
    return r


data Timeout = Timeout Int deriving (Show, Eq)

instance Exception Timeout


timeout' :: Int -> IO a -> IO (Maybe a)
timeout' t m
  | t < 0 = fmap Just m         -- if timeout < 0 just return m
  | t == 0 = return Nothing     -- if timeout is 0 nothing is done
  | otherwise = do
      pid <- myThreadId
      u <- newUnique
      let ex = Timeout (hashUnique u)
      handleJust
        (\e -> if e == ex then Just () else Nothing)
        (\_ -> return Nothing)
        (bracket (forkIO $ do threadDelay t
                              throwTo pid ex)
                 (\tid -> throwTo tid ThreadKilled)
                 (\_ -> fmap Just m))



forkFinally' :: IO a -> (Either SomeAsyncException a -> IO ()) -> IO ThreadId
forkFinally' action fun =
  mask $ \restore ->
    forkIO $ do
      r <- try (restore action)
      fun r

-- non IO code doesn't need to handle async exceoptions because there's no
-- state, so no inconsistent data.
--
-- bracket, modifyMVar, withMVar are good higher level combinators to provide
-- right async excepotion behaviour by default.
--
-- use STM we don't need to worry about async exception at all because we get
-- atomic block by default, either all changes are commited or nothing is done,
-- so no intermeidate state.
