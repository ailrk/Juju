{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import           Control.Applicative
import           Data.Function       ((&))
import           Lens

-- solve nested record.
data Person = P { _name   :: String
                , _addr   :: Address
                , _salary :: Int
                }
                deriving Show

data Address = A { _road     :: String
                 , _city     :: String
                 , _postcode :: String
                 }
                 deriving Show

setPostCode' :: String -> Person -> Person
setPostCode' pc p = p { _addr = (_addr p) { _postcode = pc } }

data LenR s a = L { viewR :: s -> a
                  , setR  :: a -> s -> s
                  , modR  :: forall f. Functor f => (a -> f a) -> s -> f s
                  }

fred = P { _name = "fred"
         , _addr = A { _road = "road1"
                     , _city = "city1"
                     , _postcode = "V1V0AA"
                     }
         , _salary = 1000
         }


name :: Lens Person String
name f (P n a s) = fmap (\n' -> P n' a s) (f n)

addr :: Lens Person Address
addr f (P n a s) = fmap (\a' -> P n a' s) (f a)

salary :: Lens Person Int
salary f (P n a s) = fmap (\s' -> P n a s') (f s)

road :: Lens Address String
road f (A r c p) = fmap (\r' -> A r' c p) (f r)

city :: Lens Address String
city f (A r c p) = fmap (\c' -> A r c' p) (f c)

postcode :: Lens Address String
postcode f (A r c p) = fmap (\p' -> A r c p') (f p)


-- ok things works
view_fred_name = view (addr . road) fred
set_fred_name = set (addr . road) "dan" fred
modify_fred_name = over (addr . road) ("not_really_" ++) fred

fred_name' = fred
           & over (addr . road) ("not_reallyy_" ++)
           & view (addr . road)

-- lens is already modifyIO, we can just give it a type here.
-- modifyIO :: Lens s a -> (a -> IO a) -> s -> IO s
-- modifyIO = id

-- modifyMaybe :: Lens s a -> (a -> Maybe a) -> s -> Maybe s
-- modifyMaybe = id

{-@ virtual field with lens @-}
data Temp = T { _fahrenheit :: Float } deriving Show

temp_here = T { _fahrenheit = 100 }

fahrenheit :: Lens Temp Float
fahrenheit fn (T f) = fmap (\f' -> T f') (fn f)

-- this is a virtual field of the original datatype.
-- it might looks like a getter for something non existed.
celsius :: Lens Temp Float
celsius fn (T f) = fmap (\c' -> T c') (fn (fToC f))

cToF :: Float -> Float
cToF c = (c * 9 / 5) + 32

fToC :: Float -> Float
fToC f = (f - 32) * 5 / 9

{-@ maintaining invariants with lens @-}
data Time = Time { _hours :: Int, _mins :: Int } deriving Show

now = Time {_hours = 3, _mins = 58}

-- not quite, but yes it shows the spirit.
mins :: Lens Time Int
mins f (Time h m) = fmap wrap (f m)
  where
    wrap :: Int -> Time
    wrap m'
      | m' >= 60 = Time { _hours = h + m' `divInt` 60
                        , _mins = m' `mod` 60
                        }

      | m' < 0  = Time { _hours = h + (abs m') `divInt` 60
                       , _mins = (abs m') `mod` 60
                       }
      | otherwise = Time h m'
    divInt m n = let x = fromIntegral m `div` fromIntegral n
                  in (fromInteger . toInteger) x

addr_strs :: Traversal' Address String
addr_strs fn (A r c p) = (\r' c' -> A r' c' p) <$> (fn r) <*> (fn c)
