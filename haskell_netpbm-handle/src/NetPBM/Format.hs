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
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module NetPBM.Format
  ( SerializeFormat(..)
  , IsPxiel(..)
  , Format(..)
  , Magic(..)
  , withFormat
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

type family Magic2FormatBin (s :: Magic) :: Format where
  Magic2FormatBin P4 = PBM
  Magic2FormatBin P2 = PGM
  Magic2FormatBin P6 = PPM

type family Magic2FormatSimple (s :: Magic) :: Format where
  Magic2FormatSimple P1 = PBM
  Magic2FormatSimple P5 = PGM
  Magic2FormatSimple P3 = PPM

type family HeaderRepDefault (s :: Magic) :: Type where
  HeaderRepDefault P4 = SimpleHeader
  HeaderRepDefault P1 = SimpleHeader
  HeaderRepDefault P2 = SimpleHeader
  HeaderRepDefault P5 = SimpleHeader
  HeaderRepDefault P3 = SimpleHeader
  HeaderRepDefault P6 = SimpleHeader

type family PixelRepDefault (s :: Magic) :: Type where
  PixelRepDefault s = PixelRep s

class SerializeFormat (s :: Magic) where
  type PixelRep s

  type HeaderRep (s :: Magic) :: Type
  type HeaderRep s = HeaderRepDefault s

  header :: (r ~ HeaderRep s) => r
  default header :: (r ~ SimpleHeader) => r
  header = simpleHeader

  serialize :: forall rep
             . (rep ~ (PixelRep s), IsPxiel rep)
            => BS.ByteString -> [rep] -> BS.ByteString
  serialize h xs = BS.concat (" " : h : fmap toByte xs)

-- monochrome format
instance SerializeFormat P4 where
  type PixelRep P4 = Word8

instance SerializeFormat P1 where
  type PixelRep P1 = Char

-- grayscale format
instance SerializeFormat P5 where
  type PixelRep P5 = Word8

instance SerializeFormat P2 where
  type PixelRep P2 = T.Text

-- color image format
instance SerializeFormat P6 where
  type PixelRep P6 = (Word8, Word8, Word8)

instance SerializeFormat P3 where
  type PixelRep P3 = (T.Text, T.Text, T.Text)


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

-- goal?
-- 1. header is polymorphic,
-- 2. we need to apply types for both serialize and header.
-- 3. we want to unify these two funcion in one.

withFormat :: (SerializeFormat m
                 , rep ~ PixelRep m
                 , IsPxiel rep
                 , h ~ HeaderRep m
                 , f ~ (BS.ByteString -> [rep] -> BS.ByteString))
              => Proxy m
              -> (h -> f -> BS.ByteString)
              -> BS.ByteString
withFormat (p :: Proxy m) k = k (header @m) (serialize @m)


-- b = header @PPM @P6 10 20 30
-- c = serialize @PPM @P6 (header @PPM @P6 1 20 30) []
