{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module BumpAllocator where

-- let's write a simple bump allocator with vector
-- bump allocator. Growth memory linearly, keep track of allocated memory, and
-- can only free memory at once.


import           Class
import           Control.Concurrent.MVar
import           Data.Vector.Generic     as GM
import           Data.Vector.Mutable     as MV
import           Foreign
import           Foreign.Storable
import           Layout

data BumpAllocator = BumpAllocator
  { heapStart   :: {-# UNPACK #-} !Word
  , heapEnd     :: {-# UNPACK #-} !Word
  , next        :: {-# UNPACK #-} !Word
  , allocations :: {-# UNPACK #-} !Word
  }


new :: BumpAllocator
new = BumpAllocator 0 0 0 0
{-# INLINE new #-}

init :: Word -> Word -> BumpAllocator
init heapStart heapSize =
  BumpAllocator heapStart (heapStart + heapSize) heapStart 0
{-# INLINE init #-}

-- align addr upwards to alignment
alignUp :: Word -> Word -> Word
alignUp addr align =
  let rem = addr `mod` align
   in if rem == 0 then addr else addr - rem + align
{-# INLINE alignUp #-}

instance Alloc (MVar BumpAllocator) where
  allocate self layout@(Layout size align) = do
    bump@BumpAllocator{..} <- takeMVar self
    let allocStart = alignUp next align
    let allocEnd = allocStart + size

    if allocEnd > heapEnd then return nullPtr else do
      let newBump = bump { next = allocEnd
                         , allocations = allocations + 1
                         }
      putMVar self newBump
      return undefined

  deallocate self ptr layout = do
    bump@BumpAllocator{..} <- takeMVar self
    putMVar self (bump { allocations = let newAlloc = allocations - 1
                                        in if allocations - 1 == 0
                                              then heapStart
                                              else newAlloc
                       })
