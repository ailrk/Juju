{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module NetPBM.Format where

import qualified Data.ByteString as BS
import           Data.Kind
import           Data.Proxy
import qualified Data.Text       as T
import           Data.Vector     as V
import           Data.Word

data Format = PBM | PPM | PGM | PNM | PAM | PM
data Magic = P1 | P2 | P3 | P4 | P5 | P6 | P7 deriving Show

class SerializeFormat (a :: Format) (s :: Magic) where
  type FormatRep a s
  type HeaderRep a s
  header :: (r ~ HeaderRep a s) => r
  serialize :: forall a s . String -> V.Vector (FormatRep a s) -> BS.ByteString

-- monochrome format
instance SerializeFormat PBM P4 where
  type FormatRep PBM P4 = Word
  type HeaderRep PBM P4 = Int -> Int -> Int -> String
  header width height maxval =
    mconcat [show width, " ", show height, " ", show maxval, " "]
  serialize = undefined

instance SerializeFormat PBM P1 where
  type FormatRep PBM P1 = Bool
  type HeaderRep PBM P1 = Int -> Int -> Int -> String
  header width height maxval =
    mconcat [show width, " ", show height, " ", show maxval, " "]
  serialize = undefined

-- grayscale format
instance SerializeFormat PGM P5 where
  type FormatRep PGM P5 = T.Text
  type HeaderRep PGM P5 = Int -> Int -> Int -> String
  header width height maxval =
    mconcat [show width, " ", show height, " ", show maxval, " "]
  serialize = undefined

instance SerializeFormat PGM P2 where
  type FormatRep PGM P2 = Bool
  type HeaderRep PGM P2 = Int -> Int -> Int -> String
  header width height maxval =
    mconcat [show width, " ", show height, " ", show maxval, " "]
  serialize = undefined


-- color image format
instance SerializeFormat PPM P6 where
  type FormatRep PPM P6 = (Word8, Word8, Word8)
  type HeaderRep PPM P6 = Int -> Int -> Int -> String
  header width height maxval =
    mconcat [show width, " ", show height, " ", show maxval, " "]
  serialize = undefined

instance SerializeFormat PPM P3 where
  type FormatRep PPM P3 = (Int, Int, Int)
  type HeaderRep PPM P3 = (Int -> Int -> Int -> String)
  header width height maxval =
    mconcat [show width, " ", show height, " ", show maxval, " "]
  serialize = undefined

-- general 2 dimensional map
-- instance Format PAM P7 where
--   type FormatRep PAM P7 = (Int, Int, Int)
--   magicWord = show P7

withFormat :: forall (s :: Format) (a :: Magic) w . w
withFormat = undefined

b = header @PPM @P6 10 20 30
c = serialize @PPM @P6 (header @PPM @P6 1 20 30) V.empty
