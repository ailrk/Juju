{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           Control.Monad.ST
import           Data.Char
import           Data.Word
import           Debug.Trace
import qualified NetPBM.Canvas    as C
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
