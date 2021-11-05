{-# LANGUAGE BangPatterns #-}
module Layout where

-- layout of a block of memory

data LayoutError = LayoutError deriving Eq

instance Show LayoutError where
  show _ = "invalid paramters to fromSizeAlign"

data Layout = Layout
  {
    -- be multiple of align.
    size  :: {-# UNPACK #-} !Word
    -- align: non zero, power of two.
  , align :: {-# UNPACK #-} !Word
  }

fromSizeAlign :: Word -> Word -> Either LayoutError Layout
fromSizeAlign size align
  | not $ align /= 0 && align `mod` 2 == 0 = Left LayoutError
  | size > fromIntegral (maxBound :: Word) - (align - 1) = Left LayoutError
  | otherwise = Right (fromSizeAlignUnchecked size align)
{-# INLINE fromSizeAlign #-}

fromSizeAlignUnchecked :: Word -> Word -> Layout
fromSizeAlignUnchecked size align = Layout size align
{-# INLINE fromSizeAlignUnchecked #-}
