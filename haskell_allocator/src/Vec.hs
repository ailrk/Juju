module Vec where

-- A c++ style vector depends on custom allocator


import Class
import Layout

data Vec a allocator = Vec a allocator
