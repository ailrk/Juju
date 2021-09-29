{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Monad.ST
import           Data.Char
import           Data.Proxy
import           Data.Word
import           Debug.Trace
import qualified NetPBM.Canvas    as C
import qualified NetPBM.Format    as F
import           Test.Hspec

main :: IO ()
main = putStrLn "hi"

test_canvas :: IO ()
test_canvas = do
  let bs = runST (C.toByteString =<< v1)
  bs `shouldBe` "aaaaaaaaa"
  where
    v1 :: ST s (C.Canvas s 3 3 Word8)
    v1 = do
      v <- C.new
      C.iforM_ v $ \(m, n) w -> do
        (Just c) <- C.index (m, n) v
        C.modify v (m, n) (+ (fromIntegral (ord 'a')))
      return v

test_format :: IO ()
test_format = do
  let b = withFormat (Proxy @P5) $ \header serialize ->
    let data = [ ' ', ' ', 'z'
               , ' ', 'z', ' '
               , 'z', ' ', ' ']
        wdata = (fromIntegral . ord) data
     in serialize (header 3 3 255) wdata
  putStrLn (show b)
  putStrLn "hi"
