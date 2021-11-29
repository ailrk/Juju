{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Hacker News, better treat it as a joke.
-- check out what are these people talking about.

import           Control.Concurrent
import           Control.Concurrent.Async

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.ByteString          as BS
import           Data.Foldable
import qualified Data.Heap                as H
import qualified Data.Text                as T
import           Data.Traversable
import           Network.HTTP.Req
import qualified Streaming                as S
import qualified Streaming.Prelude        as S
import           System.Log.FastLogger
import Control.Monad.Identity


-- fetch the first 20 pages concurrently.
-- for each page open the content and fetch all comments concurrently
-- extract comments, process to words.
-- increment word counts
-- show the result

-- key -> 1

-- O1 insert
-- Sort by some key

-- newtype BigFatMonad =

hnURL n ="https://news.ycombinator.com/news?p=" ++ show n

type BZ =  S.Stream Identity IO ()

-- download one page
download :: Int -> IO BS.ByteString
download n = runReq defaultHttpConfig $ do
  res <- req GET
             (https "news.ycombinator.com" /: "news" )
             NoReqBody
             bsResponse
             ("p" =: n)
  let body = responseBody res
  return body

-- download a list of url page
downloadN :: Int -> IO [BS.ByteString]
downloadN n = mapConcurrently (\n -> threadDelay (10^3) >> download n) $ [1..n]


main :: IO ()
main = putStrLn "Hello, Haskell!"
