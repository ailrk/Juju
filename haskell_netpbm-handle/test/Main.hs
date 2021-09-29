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

test_draw :: IO ()
test_draw = do
  let b = F.withFormat (Proxy @F.P5)
        $ \header serialize ->
          let buf = [ ' ', ' ', 'z'
                    , ' ', 'z', ' '
                    , 'z', ' ', ' ']
              wbuf = fmap (fromIntegral . ord) buf
           in serialize (header 3 3 255) wbuf
  putStrLn (show b)
