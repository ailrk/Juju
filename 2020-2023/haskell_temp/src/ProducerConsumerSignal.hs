module ProducerConsumerSignal where

-- m can be any monads.
import Control.Monad.Trans.Except
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Class
import Data.Text
import qualified Data.ByteString.Char8 as S8
import Data.Text.Encoding
import Control.Concurrent.Async

-- we want to run a single producer multiple consumer pipeline until all works
-- is done, and the the producer send a signal to close other workers.

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8


-- achieve earily loop breaking with except monad.

loop :: Monad m => ExceptT e m a -> m e
loop = fmap (either id id) . runExceptT . forever

quit :: Monad m => e ->  ExceptT e m a
quit = throwE

-- Although there is no yield point expicitly written in the code, the compiler
-- will insert yeild point so the control can be given away.

runWorks :: IO ()
runWorks = do
  chan <- newChan
  let workers = mapConcurrently (worker chan) [1..5]
      producer = mapM_ (writeChan chan) ([1..100] ++ [(-1)])
  workers `concurrently` producer
  return ()
  where
    sendMsg = lift . say . pack . mconcat
    worker :: Chan Int -> Int -> IO ()
    worker chan num = loop $ do
      i <- lift $ readChan chan
      when (i == (-1)) $ do     -- little sentinel
        lift $ writeChan chan (-1)
        quit ()
      sendMsg $ [ "Worker #" , show num , " received value " , show i ]
