{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase  #-}
module Actor where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.IO

import Debug.Trace


-- Actor model
--  - each process has an id.
--  - sending and receiving messages are ansynchronous.
--  - each process holding it's own private state.
--
-- Pros:
--  - no manual multithreaded programming with locks because no shared memory.
--    Which means programm will be deadlock free.
--  - fault tolerant. A supervisor can reboot crashed actors.
--  - good for distributed programming.
--
-- Cons:
--  - Sometime shared memory is necessary.
--  - All communications are async means it's hard to maintain order.
--  - hard to debug, cause now you have several separated processes instead of
--    one.


data ActorRef msg
  = ActorRef
    { refId :: ThreadId
    , refMbox :: TQueue msg
    } deriving Eq


newtype Behaviour msg = Behaviour { getBehaviour :: msg -> IO (Behaviour msg) }


spawn :: Behaviour msg -> IO (ActorRef msg)
spawn b = do
  refMbox <- newTQueueIO
  let go (Behaviour b') = void $ do
        msg <- atomically $ readTQueue refMbox
        b' msg >>= go
  refId <- forkIO $ go b
  return $ ActorRef{..}


send :: ActorRef msg -> msg -> IO ()
send ActorRef{..} msg = atomically $ writeTQueue refMbox msg


data FileReaderMsg
  = OpenFile FilePath
  | SendLine (ActorRef FileReaderMsg)
  | GotLine String
  | CloseFile
  deriving Eq


fileReader :: Behaviour FileReaderMsg
fileReader = whenClosed
  where
    whenClosed = Behaviour $ \case
      OpenFile fp -> do
        h <- openFile fp ReadMode
        return (whenOpened h)
      CloseFile -> return whenClosed
      _ -> error "impossible state"
    whenOpened h = Behaviour $ \case
      SendLine actor -> do
        line <- hGetLine h
        actor `send` GotLine line
        return $ whenOpened h
      CloseFile -> do
        hClose h
        return (whenClosed)
      _ -> error "impossible state"


data StdoutWriterMsg = WriteString String

lineWriter :: Behaviour FileReaderMsg
lineWriter = Behaviour $ \case
  GotLine str -> do
    putStrLn str
    return lineWriter
  _ -> error "impossible state"


run :: IO ()
run = do
  fileReaderActor <- spawn fileReader
  fileReaderActor `send` OpenFile "haskell-concurrency.cabal"

  lineWriterActor <- spawn lineWriter

  fileReaderActor `send` SendLine lineWriterActor
