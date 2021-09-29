{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeApplications    #-}
module NetPBM.Canvas
  ( Canvas
  , new
  , size
  , toByteString
  , index
  , modify
  , imapM_
  , iforM_
  , foldM
  ) where


import           Control.Monad.ST
import qualified Data.ByteString     as BS
import           Data.Kind
import           Data.Proxy
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as M
import           Data.Word
import           Debug.Trace
import           GHC.TypeLits
import           NetPBM.Format

-- a canvas is a binary buffer holds characters.
-- it holds a mutable array,
-- The entire canvas will be in memory so this module is strict.

-- width and height should known statically.

data Canvas s (width :: Nat) (height :: Nat) (pixel :: Type) where
  Canvas :: IsPxiel pixel => M.STVector s pixel -> Canvas s width height pixel

type IsCanvas width height pixel =
  (KnownNat width, KnownNat height, IsPxiel pixel)

new :: forall width height pixel s
     . IsCanvas width height pixel
    => ST s (Canvas s width height pixel)
new = let m = natVal (Proxy @width)
          n = natVal (Proxy @height)
       in Canvas <$> (M.generate (fromIntegral (m * n)) (const defaultPixelRep))

size :: IsCanvas width height pixel
     => Canvas s width height pixel -> Int
size (Canvas v) = M.length v

toByteString :: IsCanvas width height pixel
             => Canvas s width height pixel -> ST s BS.ByteString
toByteString (Canvas buffer) = do
  vs <- V.freeze $ buffer
  return $ BS.concat . fmap toByte . V.toList  $ vs

index :: IsCanvas width height pixel
      => (Int, Int)
      -> Canvas s width height pixel -> ST s (Maybe pixel)
index (m, n) c@(Canvas buffer) =
  let i = m * n
      sz = size c
   in if i > sz
         then return Nothing
         else Just <$> M.read buffer i

modify :: forall width height pixel s
        . IsCanvas width height pixel
       => Canvas s width height pixel
       -> (Int, Int)
       -> (pixel -> pixel)
       -> ST s ()
modify (Canvas buffer) (m, n) action  =
  let w = natVal (Proxy @width)
   in M.modify buffer action (m * fromInteger w + n)

imapM_ :: forall width height m s pixel b
        . IsCanvas width height pixel
      => ((Int, Int) -> pixel -> ST s b)
      -> Canvas s width height pixel
      -> ST s ()
imapM_ f (Canvas buffer) =
  let m = natVal (Proxy @width)
      n = natVal (Proxy @height)
   in M.imapM_ (\i n -> let (d, r) = i `quotRem` (fromIntegral m)
                       in f (d, r) n)
               buffer

iforM_ :: IsCanvas width height pixel
      => Canvas s width height pixel
      -> ((Int, Int) -> pixel -> ST s b)
      -> ST s ()
iforM_ = flip imapM_

foldM :: IsCanvas width height pixel
      => (b -> pixel -> ST s b)
      -> b
      -> Canvas s width height pixel
      -> ST s b
foldM f b (Canvas c) = M.foldM f b c

copy :: (KnownNat width, KnownNat height)
     => Canvas s width height pixel
     -> Canvas s width height pixel
     -> ST s ()
copy (Canvas tgtBuf) (Canvas srcBuf) = do M.copy tgtBuf srcBuf
