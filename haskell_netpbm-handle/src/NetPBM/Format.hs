{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module NetPBM.Format
  ( SerializeFormat(..)
  , IsPxiel(..)
  , Format
  , Magic
  ) where

import qualified Data.ByteString    as BS
import           Data.Char          (ord)
import           Data.Kind
import           Data.Proxy
import           Data.String
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import           Data.Vector        as V
import           Data.Word

data Format = PBM | PPM | PGM | PNM | PAM | PM
data Magic = P1 | P2 | P3 | P4 | P5 | P6 | P7 deriving Show

-------------------------------------------------------------------------------

type family HeaderRepDefault (a :: Format) (s :: Magic) :: Type where
  HeaderRepDefault PBM P4 = SimpleHeader
  HeaderRepDefault PBM P1 = SimpleHeader
  HeaderRepDefault PGM P2 = SimpleHeader
  HeaderRepDefault PGM P5 = SimpleHeader
  HeaderRepDefault PPM P3 = SimpleHeader
  HeaderRepDefault PPM P6 = SimpleHeader

type family PixelRepDefault (a :: Format) (s :: Magic) :: Type where
  PixelRepDefault a s = PixelRep a s

class SerializeFormat (a :: Format) (s :: Magic) where
  type PixelRep a s

  type HeaderRep (a :: Format) (s :: Magic) :: Type
  type HeaderRep a s = HeaderRepDefault a s

  header :: (r ~ HeaderRep a s) => r
  default header :: (r ~ SimpleHeader) => r
  header = simpleHeader

  serialize :: forall rep . (rep ~ (PixelRep a s), IsPxiel rep)
            => BS.ByteString -> [rep] -> BS.ByteString
  serialize h xs = BS.concat (h : fmap toByte xs)

-- monochrome format
instance SerializeFormat PBM P4 where
  type PixelRep PBM P4 = Word8

instance SerializeFormat PBM P1 where
  type PixelRep PBM P1 = Char

-- grayscale format
instance SerializeFormat PGM P5 where
  type PixelRep PGM P5 = Word8

instance SerializeFormat PGM P2 where
  type PixelRep PGM P2 = T.Text

-- color image format
instance SerializeFormat PPM P6 where
  type PixelRep PPM P6 = (Word8, Word8, Word8)

instance SerializeFormat PPM P3 where
  type PixelRep PPM P3 = (T.Text, T.Text, T.Text)


-- header with w h m format
type SimpleHeader = Int -> Int -> Int -> BS.ByteString
simpleHeader :: SimpleHeader
simpleHeader width height maxval =
      BS.pack . fmap (fromIntegral . ord)
    $ mconcat [show width, " ", show height, " ", show maxval, " "]


-------------------------------------------------------------------------------

class IsPxiel r where
  defaultPixelRep :: r
  toByte :: r -> BS.ByteString

instance IsPxiel Word8 where
  defaultPixelRep = 0 :: Word8
  toByte r = BS.singleton r

instance IsPxiel Char where
  defaultPixelRep = '0'
  toByte = BS.singleton . fromIntegral . ord

instance IsPxiel T.Text where
  defaultPixelRep = "0"
  toByte = T.encodeUtf8

instance IsPxiel (Word8, Word8, Word8) where
  defaultPixelRep = (0, 0, 0)
  toByte (a, b, c) = BS.concat [toByte a, toByte b, toByte c]

instance IsPxiel (T.Text, T.Text, T.Text) where
  defaultPixelRep = ("0", "0", "0")
  toByte (a, b, c) = BS.concat [toByte a, toByte b, toByte c]

withFormat :: forall (s :: Format) (a :: Magic) w . w
withFormat = undefined

-- b = header @PPM @P6 10 20 30
-- c = serialize @PPM @P6 (header @PPM @P6 1 20 30) []
