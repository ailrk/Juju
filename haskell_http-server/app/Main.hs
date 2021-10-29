{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where
import           Network.HTTP.Types            as HTTP
import           Network.Wai                   as Wai
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Streaming.ByteString          as Streaming


import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Carrier.Writer.Strict
import Data.Functor.Of (Of((:>)))

main :: IO ()
main = Warp.run 8080 (runApp hello)


-- type App = Wai.Request -> (Wai.Response -> IO ResponseReceived)
--          -> IO ResponseReceived
type ByteStream = Streaming.ByteStream IO

-- reading requests, read write http status, output response header, and
-- response with byte stream.
-- Note ByteStream m s is a monad provided by streaming-bytestring. To use it
-- as an effect we need to lift the effect.

-- fused effect allows multiple overlapped effects, to differentiate we need
-- to visibly apply types to tell which effect we are applying to.

hello :: ( Has (Reader Wai.Request) sig m
         , Has (State HTTP.Status) sig m
         , Has (Writer ResponseHeaders) sig m
         , Has (Lift ByteStream) sig m
         )
           => m ()
hello = do
  req <- ask @Wai.Request
  tell @ResponseHeaders [(HTTP.hContentType, "text/plain")]
  sendM @ByteStream "Hello World\n"
  sendM @ByteStream ("Request: " <> Streaming.fromStrict (Wai.rawQueryString req))
  put @HTTP.Status HTTP.ok200

runApp ::  _ () -> Wai.Application
runApp action req respond = do
  result <-
    Streaming.toLazy
    . runM @ByteStream
    . runReader @Wai.Request req
    . runState @HTTP.Status HTTP.status500
    . runWriter @Wai.Response
    $ action
  let (respBody :> (status, (_, ()))) = result
  respond (Wai.responseLBS status headers respBody)
