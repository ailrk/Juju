module Class where


import Control.Monad
import           Foreign
import           Layout
-- import Foreign.Marshal.Array


class Alloc a where
  allocate        :: a -> Layout -> IO (Ptr Word8)
  deallocate      :: a -> Ptr Word8 -> Layout -> IO ()

  -- allocate and init values to 0
  allocate_zeroed :: a -> Layout -> IO (Ptr Word8)
  allocate_zeroed self layout@(Layout size align) = do
    ptr <- allocate self layout
    when (ptr /= nullPtr) $ pokeArray ptr [0..]
    return ptr

  -- shrink or grow a block of memory
  reallocate      :: a -> Ptr Word8 -> Layout -> Word -> IO (Ptr Word8)
  reallocate self ptr layout@(Layout size align) newSize
    | newLayout <- fromSizeAlignUnchecked newSize align = do
        newPtr <- allocate self newLayout
        when (newPtr /= nullPtr) $ deallocate self ptr layout
        return newPtr
