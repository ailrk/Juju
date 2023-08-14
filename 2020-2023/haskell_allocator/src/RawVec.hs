module RawVec where

-- A low-level for low level memory management. Simple abstraction over
-- allcators.


import Class
import Layout

data AllocInit = Unitialized | Zeroed
