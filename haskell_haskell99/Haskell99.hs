module MiniProject.Haskell99 where

import           Data.Foldable
import           Data.List
import           Data.Traversable

import           Control.Monad.ST
import           Data.IORef
import           Data.STRef
import           Debug.Trace
import           System.Random

import           Data.Ord

import           Data.Maybe

import           Data.Function

import           Control.Applicative
import           Control.Monad

import           System.IO.Unsafe    (unsafePerformIO)

import           Test.Hspec


{-@ Question 1 to 10 List
@-}

-- 1.   ----------------------------------------
-- ---------------------------------------------
-- (*) Find the last element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)

-- >>> last_ [1..10]
-- 10
last_ :: [a] -> a
last_ []     = error "oh"
last_ [a]    = a
last_ (x:xs) = last_ xs

-- >>> last_' [1..10]
-- 10
last_' :: [a] -> a
last_' = foldr1 (flip const)

-- >>> last_'' ['a'..'z']
-- 'z'
last_'' :: [a] -> a
last_'' = head . reverse

-- >>> last_'' ['a'..'z']
-- 'z'
last_''' :: [a] -> a
last_''' = foldr1 (curry snd)

-- 2.   ----------------------------------------
-- ---------------------------------------------
-- (*) Find the last but one element of a list.

-- >>> butLast_ [1..10]
-- 9
butLast_ :: [a] -> a
butLast_ []       = error "oh"
butLast_ [a]      = error "oh"
butLast_ (a:x:[]) = a
butLast_ (x:xs)   = butLast_ xs


-- >>> butLast_' [1..10]
-- 9
butLast_' :: [a] -> a
butLast_' = last . init


-- >>> butLast_'' [1..10]
-- 9
butLast_'' :: [a] -> a
butLast_'' = head . tail . reverse


-- 3.   ----------------------------------------
-- ---------------------------------------------
-- (*) Find the K'th element of a list. The first element in the list is number 1.

-- >>> elementAt_ [1..10] 2
-- 3
-- >>> elementAt_ [1..10] (-1)
-- index can't be smaller than 0
elementAt_ :: [a] -> Int -> a
elementAt_ [] _ = error "empty list"
elementAt_ (x:xs) n
  | n == 0 = x
  | n < 0 = error "index can't be smaller than 0"
  | otherwise = elementAt_ xs (n - 1)

-- >>> elementAt_' [1..10] 2
-- 3
elementAt_' :: [a] -> Int -> a
elementAt_' xs n = xs !! n


-- >>> elementAt_'' [1..10] 2
-- 2
elementAt_'' :: [a] -> Int -> a
elementAt_'' xs n
  | n < 0 = error "n should be larger than 0"
  | otherwise = snd . head . dropWhile (\(idx, _) -> idx < n) $ (zip [1..] xs)


-- 4.   ----------------------------------------
-- ---------------------------------------------
-- (*) Find the number of elements of a list.

-- >>> length_ [1..10]
-- 10
length_ :: [a] -> Int
length_ []     = 0
length_ (x:xs) = 1 + (length_ xs)

-- >>> length_' [1..10]
-- 10
length_' :: [a] -> Int
length_' = foldr (\_ b -> b + 1) 0

-- >>> length_'' [1..10]
-- 10
length_'' :: [a] -> Int
length_'' = sum . (fmap $ const 1)


-- 5.   ----------------------------------------
-- ---------------------------------------------
-- (*) Reverse a list.

-- >>> reverse_ [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_ :: [a] -> [a]
reverse_ []     = []
reverse_ (x:xs) = (reverse_ xs) ++ [x]

-- >>> reverse_' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_' :: [a] -> [a]
reverse_' = foldr (\a b -> b ++ [a]) []

-- >>> reverse_'' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_'' :: [a] -> [a]
reverse_'' = foldl (\b a -> a:b) []

-- >>> reverse_''' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_''' :: [a] -> [a]
reverse_''' xs = runST $ do
  listRef <- sequence $ fmap newSTRef xs
  let len = length_ xs
      mid = (len `div` 2) - 1
  traverse_ (\i -> swap (listRef !! i) (listRef !! (len-1-) i)) [0..mid]
  traverse readSTRef listRef
  where
    swap :: STRef s a -> STRef s a -> ST s ()
    swap ref1 ref2 = do
      r1 <- readSTRef ref1
      r2 <- readSTRef ref2
      writeSTRef ref2 r1
      writeSTRef ref1 r2
      return ()


-- 6.   ----------------------------------------
-- ---------------------------------------------
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward;
--     e.g. (x a m a x).

-- >>> isPalindrome [1, 2, 3, 2, 1]
-- True
-- >>> isPalindrome [1, 2, 3, 2, 1, 2]
-- False
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- >>> isPalindrome' [1, 2, 3, 2, 1]
-- True
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' = liftA2 (==) id reverse


-- 7.   ----------------------------------------
-- ---------------------------------------------
--  (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into
-- a `flat' list by replacing each list with its elements (recursively).

data NestedList a = E a | L [NestedList a]

-- >>> flattern (E 5)
-- [5]
--
-- >>> flattern (L [E 1, L [E 2, L [E 3, E 4], E 5]])
-- [1,2,3,4,5]
--
-- >>> flattern (L [])
-- []
flattern :: NestedList a -> [a]
flattern (L [])        = []
flattern (E x)         = [x]
flattern (L (E x: xs)) = x : (mconcat . fmap flattern $ xs)


-- concatMap map f to each elements and concat the result.
flattern' :: NestedList a -> [a]
flattern' (E x)  = [x]
flattern' (L xs) = concatMap flattern' xs

-- foldMap can be thought as the generalization of concatMap.
flattern'' :: NestedList a -> [a]
flattern'' (E x)  = [x]
flattern'' (L xs) = foldMap flattern'' xs

-- bind for list is just concatMap.
flattern''' :: NestedList a -> [a]
flattern''' (E x)  = return x
flattern''' (L xs) = xs >>= flattern'''



-- 8.   ----------------------------------------
-- ---------------------------------------------
-- (**) Eliminate consecutive duplicates of list elements.

-- >>> compress "aaaabccaadeeee"
-- "abcade"
--
compress :: Eq a => [a] -> [a]
compress = foldr op []
  where
    op a b
      | b == [] = a : b
      | otherwise = case b of
                      x:xs -> if x == a then x:xs else a:x:xs


-- >>> compress' "aaaabccaadeeee"
-- "abcade"

compress' :: Eq a => [a] -> [a]
compress' []          = []
compress' t@(x:xs) = let rest = (dropWhile (==x) t)
                      in x:compress' rest

-- >>> compress' "aaaabccaadeeee"
-- "abcade"
compress'' :: Eq a => [a] -> [a]
compress'' = map head . group



-- 9.   ----------------------------------------
-- ---------------------------------------------
-- (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack = foldr op []
  where
    op a [] = [[a]]
    op a (b:bs)
      | a == head b = (a : b) : bs
      | otherwise = [a] : (b:bs)

-- span separate a list into two, the first half stop at the first element satisfies
-- predicate.
--
-- NOTE: A typical use case is to recursively separate a list until empty.
--
-- >>> pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack' :: Eq a => [a] -> [[a]]
pack' (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack rest
pack' [] = []


-- 10.  ----------------------------------------
-- ---------------------------------------------
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates of
-- elements are encoded as lists (N E) where N is the number of duplicates of the element E.

-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--
encode :: Eq a => [a] -> [(Int, a)]
encode = foldr op []
  where
    op a [] = [(1, a)]
    op a xs@((i, b):bs)
      | a == b = (i + 1, b) : bs
      | otherwise = (1, a) : xs


-- >>> encode' "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode' :: Eq a => [a] -> [(Int, a)]
encode' = fmap (\a -> (length a, head a)) . group

-- use applicative to partially apply
-- >>> encode'' "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--
encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = fmap ((,) <$> length <*> head) . group

{-@ Question 11 to 20 Lists, continued
@-}

-- 11.  ----------------------------------------
-- ---------------------------------------------
-- (*) Modified run-length encoding.


-- >>> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
--
data Dup a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Dup a]
encodeModified = fmap f . group
  where
    f a
      | length a == 1 =  Single . head $ a
      | otherwise = Multiple (length a) (head a)

-- >>> encodeModified' "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeModified' :: Eq a => [a] -> [Dup a]
encodeModified' = fmap f . encode
  where
    f (1, x) = Single x
    f (n, x) = Multiple n x


-- 12.  ----------------------------------------
-- ---------------------------------------------
-- (**) Decode a run-length encoded list.

-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified [] = []
decodeModified (Single x:xs) = x:decodeModified xs
decodeModified ((Multiple 2 x):xs) = x : decodeModified ((Single x):xs)
decodeModified ((Multiple n x):xs) = x : decodeModified ((Multiple (n - 1) x):xs)

-- >>> decodeModified' [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified' :: [Dup a] -> [a]
decodeModified' = concatMap f
  where
    f (Single x)     = [x]
    f (Multiple n x) = replicate n x


-- 13.  ----------------------------------------
-- ---------------------------------------------
-- (**) Run-length encoding of a list (direct solution).

-- >>> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
--
encodeDirect :: Eq a => [a] -> [Dup a]
encodeDirect = foldr op []
  where
    op a []                = [Single a]
    op a xs@((Single b):bs)
      | a == b = (Multiple 2 a) : bs
      | otherwise = (Single a):xs
    op a xs@((Multiple n b):bs)
      | a == b = (Multiple (n + 1) a) : bs
      | otherwise = (Single a):xs


-- 14.  ----------------------------------------
-- ---------------------------------------------
-- (*) Duplicate the elements of a list.

-- >>> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = x:x:dupli xs

-- >>> dupli' [1, 2, 3]
-- [1,1,2,2,3,3]
dupli' :: [a] -> [a]
dupli' = foldr (\a b -> a:a:b) []

-- >>> dupli'' [1, 2, 3]
-- [1,1,2,2,3,3]
dupli'' :: [a] -> [a]
dupli'' = foldMap (\a -> [a, a])

-- NOTE: monad for list is foldMap / concatMap
-- >>> dupli''' [1, 2, 3]
-- [1,1,2,2,3,3]
dupli''' :: [a] -> [a]
dupli''' xs = xs >>= \x -> [x, x]

-- NOTE: apply applicative over functions is a very common technique
--       Note <*> for function:
--        f <*> g => \x -> (f x) (g x)
--        ap f g x = ap (f x) (g x)
--        both function f, g waiting for the same input x.
--        the result of (g x) is used as paramter of (f x)
--
--          ((.) <$> (:) <*> (:))
--       => ((.) . (:) <*> (:))
--       => (\x -> (.) (x:)) <*> (:)
--       => \y -> (\x -> (.) (x:)) y (y:)
--       => \y -> ((.) (y:)) (y:)
--       => \y -> (y:) (y:)
--       => \y -> \z -> (y:) ((y:) z)
--       => y z y:y:z
--
-- >>> dupli'''' [1, 2, 3]
dupli'''' :: [a] -> [a]
dupli'''' = foldr ((.) <$> (:) <*> (:)) []


-- 15.  ----------------------------------------
-- ---------------------------------------------
-- (**) Replicate the elements of a list a given number of times.

-- >>> repli "abc" 3
-- "aaaabbbbcccc"
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs n = foldMap (go n) xs
  where
    go 0 x = [x]
    go n x = x : go (n - 1) x

-- feels like cheating
-- >>> repli' "abc" 3
-- "aaabbbccc"
--
repli' :: [a] -> Int -> [a]
repli' xs n = foldMap (replicate n) xs

-- >>> repli'' "abc" 3
-- "aaabbbccc"
repli'' :: [a] -> Int -> [a]
repli'' xs n = xs >>= replicate n

-- fold is really powerful..?!
-- >>> repli''' "abc" 3
-- "aaabbbbcccc"
repli''' :: [a] -> Int -> [a]
repli''' [] _     = []
repli''' (x:xs) n = foldr (const (x:)) (repli xs n) [1..n]


-- 16.  ----------------------------------------
-- (**) Drop every N'th element from a list.

-- >>> dropEvery ['a'..'z'] 3
-- "abdeghjkmnpqstvwyz"
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n - 1) xs ++ dropEvery (drop n xs) n


-- 17.  ----------------------------------------
-- (*) Split a list into two parts; the length of the first part is given.

-- >>> split [1..10] 3
-- ([1,2,3],[4,5,6,7,8,9,10])
--
split :: [a] -> Int -> ([a], [a])
split xs = go ([], xs)
  where
    go :: ([a], [a]) -> Int -> ([a], [a])
    go (acc, xs) 0   = (acc, xs)
    go (acc, x:xs) n = go (acc ++ [x], xs) (n - 1)

-- >>> split' [1..10] 3
-- ([1,2,3],[1,2,3,4,5,6,7,8,9,10])
split' :: [a] -> Int -> ([a], [a])
split' xs n = let xs' = (zip xs [1..])
                  lpred = (<=n) . snd
                  rpred = (>n) . snd
               in (,) <$> fmap fst . takeWhile lpred <*> fmap fst . dropWhile rpred $ xs'

-- >>> split'' [1..10] 3
-- ([1,2,3],[4,5,6,7,8,9,10])
split'' :: [a] -> Int -> ([a], [a])
split'' xs n = (take n xs, drop n xs)


-- 18.  ----------------------------------------
-- (**) Extract a slice from a list.

-- the question asking for slice from 1, a bit weird.

-- >>> slice ['a'..'z'] 3 7
-- "cdefg"
-- >>> slice ['a'..'z'] 0 7
-- invalid range
-- >>> slice ['a'..'z'] 1 7
-- "abcdefg"
-- >>> slice ['a'..'z'] 2 7
-- "bcdefg"
--
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs l r = take (r-l+1) . drop (l - 1) $ xs

-- >>> slice' ['a'..'z'] 3 7
-- "cdefg"
slice' :: [a] -> Int -> Int -> [a]
slice' xs l r
  | l < 1 || r < 1 || r < l  = error "invalid range"
  | r - l > length xs = error "range too large"
slice' xs l r = let (_, rest) =  splitAt (l - 1) xs
                    (initial, _) = splitAt (r - l + 1) rest
                 in initial

-- >>> slice'' ['a'..'z'] 3 7
-- "cdefg"
-- >>> slice'' ['a'..'z'] 0 7
-- invalid range
-- >>> slice'' ['a'..'z'] 1 7
-- "abcdefg"
-- >>> slice'' ['a'..'z'] 2 7
-- "bcdefg"
slice'' :: [a] -> Int -> Int -> [a]
slice'' xs l r
  | l < 1 || r < 1 || r < l  = error "invalid range"
  | r - l > length xs = error "range too large"
slice'' xs l r = fmap (fst)
               . filter ((\i -> i >= l && i <= r ) . snd)
               $ (zip xs [1..])


-- 19.  ----------------------------------------
-- (**) Rotate a list N places to the left.

-- >>> rotate ['a'..'g'] 3
-- "defgabc"
--
rotate :: [a] -> Int -> [a]
rotate xs n = slice xs (n + 1) (length xs) ++ slice xs 1 n

-- >>> rotate' ['a'..'g'] 3
-- "defgabc"
rotate' :: [a] -> Int -> [a]
rotate' xs n = let (ls, rs) = splitAt n xs
                in rs ++ ls

-- >>> rotate'' ['a'..'g'] 3
-- "defgabc"
rotate'' :: [a] -> Int -> [a]
rotate'' xs n = let (l, r) = go [] xs n
                 in l ++ reverse r
  where
    go acc xs 0     = (xs, acc)
    go acc (x:xs) n = go (x:acc) xs (n - 1)


-- 20.  ----------------------------------------
-- (*) Remove the K'th element from a list.


-- >>> removeAt [1..10] 3
-- [1,2,3,5,6,7,8,9,10]
--
removeAt :: [a] -> Int -> [a]
removeAt [] _     = []
removeAt (x:xs) 0 = xs
removeAt (x:xs) n = x : removeAt xs (n - 1)

-- >>> removeAt' [1..10] 3
-- [1,2,3,5,6,7,8,9,10]
--
removeAt' :: [a] -> Int -> [a]
removeAt' xs n = let (ls, rs) = splitAt n xs
                  in ls ++ tail rs

-- >>> removeAt'' [1..10] 3
-- [1,2,3,5,6,7,8,9,10]
--
removeAt'' :: [a] -> Int -> [a]
removeAt'' xs n = fmap fst . filter ((/=(n+1)) . snd) $ zip xs [1..]


{-@ Question 21 to 28 List again
@-}

-- 21.  ----------------------------------------
-- Insert an element at a given position into a list.

-- TODO one off.
-- >>> insertAt 'X' "abcd" 2
-- "aXbcd"
--
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (ls, rs) = splitAt (n-1) xs
                   in ls ++ (x:rs)

-- 22.  ----------------------------------------
-- Create a list containing all integers within a given range.

-- avoid use length when work with infinite list.
-- >>> range 4 9
-- [4,5,6,7,8,9]
--
range :: Int -> Int -> [Int]
range l r = slice [1..] l r

-- >>> range' 4 9
-- [4,5,6,7,8,9]
range' :: Int -> Int -> [Int]
range' l r
  | l == r = [r]
  | l < r = l : range (l + 1) r
  | otherwise = error "invalid"

-- >>> range'' 4 9
-- [5,6,7,8,9]
range'' :: Int -> Int -> [Int]
range'' l r = take (r - l) . drop l $ [1..]


-- 23.  ----------------------------------------
-- Extract a given number of randomly selected elements from a list.

-- oh I guess the quesition is asking non replace selecting, well,
-- >>> unsafePerformIO $ rndSelect ['a'..'f'] 3
-- "bcc"
-- "ccb"
-- "dae"
--
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
  | n < 0 = error "n can't be negative"
  | otherwise = do
    idxs <- sequence [randomRIO (0, length xs - 1) | _ <- [1..n]]
    return $ fmap (xs !!) idxs


-- >>> unsafePerformIO $ rndSelect' ['a'..'f'] 3
-- "aec"
-- "bcf"
-- "bbd"
--
rndSelect' :: [a] -> Int -> IO [a]
rndSelect' [] _ = return []
rndSelect' xs n
  | n < 0 = error "n can't be negative"
  | otherwise = do
    idxs <- replicateM n . getStdRandom $ randomR (0, length xs - 1)
    return [xs !! i | i <- idxs]


-- 24.  ----------------------------------------
-- Lotto: Draw N different random numbers from the set 1..M.

-- replicateM can be more useful than traverse sometimes.
-- it's like dotimes
-- >>> diffSelect 10 60
-- [6,51,29,22,26,45,30,15,26,46]
-- [53,53,57,51,2,42,9,29,17,47]
--
diffSelect :: Int -> Int -> IO [Int]
diffSelect n ub = replicateM n (randomRIO (1, ub))

-- >>> diffSelect' 10 60
-- [22,11,33,18,58,43,28,7,15,6]
-- [45,30,52,35,4,32,33,15,19,36]
--
diffSelect' :: Int -> Int -> IO [Int]
diffSelect' n ub = replicateM n . getStdRandom $ randomR (1, ub)

-- >>> diffSelect'' 10 60
-- [18,48,60,17,38,43,48,57,20,35]
-- [39,18,50,11,39,59,50,45,41,22]
--
diffSelect'' :: Int -> Int -> IO [Int]
diffSelect'' n ub = traverse (const . randomRIO $ (1, ub)) [1..n]

-- >>> diffSelect''' 10 60
diffSelect''' :: Int -> Int -> IO [Int]
diffSelect''' n ub = sequence (fmap (\a -> randomRIO (1, ub)) [1..n])



-- 25.  ----------------------------------------
-- Generate a random permutation of the elements of a list.

-- use pure combinators we made so far.
-- >>> rndPermu [1..10]
-- [3,7,5,8,9,1,10,2,4,6]
-- [1,3,2,5,7,4,8,9,6,10]
-- [6,1,3,4,8,5,9,2,7,10]
rndPermu :: [a] -> IO [a]
rndPermu xs = do
  idxs <- traverse (const . randomRIO $ (0, size)) [1..size]
  return $ go xs idxs
  where
    size = length xs
    shuffle' xs j = let x = head xs
                     in insertAt x (removeAt xs 0) j
    go xs []      = xs
    go xs (i:idx) = go (shuffle' xs i) idx


-- >>> rndPermu' [1..10]
-- [10,9,7,4,5,8,1,6,3,2]
-- [7,2,3,4,8,6,1,5,9,10]
-- [1,6,9,2,8,3,5,7,4,10]
rndPermu' :: [a] -> IO [a]
rndPermu' xs = do
  let n = length xs - 1
  xsRef <- traverse newIORef xs
  replicateM_ n $ do
    i <- randomRIO (0, n)
    j <- randomRIO (0, n)
    x <- readIORef (xsRef !! i)
    y <- readIORef (xsRef !! j)
    writeIORef (xsRef !! j) x
    writeIORef (xsRef !! i) y
  traverse readIORef xsRef



-- 26.  ----------------------------------------
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
-- 1 2 3 4 5
-- 1 2 3 / 1 2 4 / 1 2 5 / 1 3 4 / 1 3 5 / 1 4 5
-- 2 3 4 / 2 3 5 / 2 4 5
-- 3 4 5


-- >>> combinations 3 [1..5]
-- [[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
--
combinations :: Int -> [a] -> [[a]]
combinations 1 xs = fmap (:[]) xs
combinations n t@(x:xs)
  | length t < n = []
  | otherwise = fmap (x:) (combinations (n - 1) xs) ++ (combinations n xs)


-- >>> combinations' 3 [1..5]
-- [[1,2,3],[1,2,4],[1,3,4],[2,3,4],[1,2,5],[1,3,5],[2,3,5],[1,4,5],[2,4,5],[3,4,5]]
combinations' :: Int -> [a] -> [[a]]
combinations' n xs = filter ((n==) . length) (subsequences xs)


-- >>> combinations'' 3 [1..5]
-- [[1,2,3],[1,2,4],[1,2,5],[1,3,4],[1,3,5],[1,4,5],[2,3,4],[2,3,5],[2,4,5],[3,4,5]]
--
combinations'' :: Int -> [a] -> [[a]]
combinations'' _ [] = [[]]
combinations'' 0 _ = [[]]
combinations'' n (x:xs) = let begin = [x : rest | rest <- combinations'' (n - 1) xs]
                              rest = if n <= length xs then combinations'' n xs else []
                           in begin ++ rest


-- 27.  ----------------------------------------
-- Group the elements of a set into disjoint subsets.

-- >>> group' [2,3,4] ["aldo","beat","carla","david"]
-- [[["aldo","beat"],["aldo","carla"],["aldo","david"],["beat","carla"],["beat","david"],["carla","david"]],[["aldo","beat","carla"],["aldo","beat","david"],["aldo","carla","david"],["beat","carla","david"]],[["aldo","beat","carla","david"]]]
group' :: [Int] -> [a] -> [[[a]]]
group' ns xs =  fmap (flip combinations $ xs) ns


-- 28.  ----------------------------------------
-- Sorting a list of lists according to length of sublists

-- Naive approach. if you're really out of idea, at least one thing you can
-- do is to tag information as a tuple.
-- Doing so the code will become messier, but it's easier to come up a solution this
-- way. You can access more information.
-- >>> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort xs = fmap snd . sortBy (compare `on` fst)  $ (zip (fmap length xs)  xs)

-- >>> lsort' ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
lsort' :: [[a]] -> [[a]]
lsort' = sortBy (comparing length)


-- >>> lsort'' ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl"]
--
lsort'' :: [[a]] -> [[a]]
lsort'' = sortBy (compare `on` length)


-- this is really hard to debug tbh...
-- again, keeping too much information makes the code more complicated than it
-- should be.
-- on takes (a -> a -> c) and a (a -> b), and convert the binop into (b -> b -> c)
--
-- >>> lfsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","ijkl","abc","fgh","de","de","mn"]
lfsort :: [[a]] -> [[a]]
lfsort = fmap snd
       . sortBy (compare `on` fst)
       . mconcat
       . fmap (\xs -> fmap (\ys -> (length xs,  snd ys)) xs)
       . groupBy ((==) `on` fst)
       . sortBy (compare `on` fst)
       . fmap ((,) <$> length <*> id)


-- >>> lfsort ["abc","de","fgh","de","ijkl","mn","o"]
lfsort' :: [[a]] -> [[a]]
lfsort' = mconcat . lsort' . groupBy ((==) `on` length) . lsort



{-@ Question 31 to 40 Arihtmeic
@-}
-- 31.  ----------------------------------------
-- (**) Determine whether a given integer number is prime.


-- Primality test is really based on cases.
-- You need to break it down and consider different possiblities separately.
-- >>> filter ((==) <$> snd <*> const True)(zip [1..20] (isPrime <$> [1..20]))
-- [(2,True),(3,True),(5,True),(7,True),(11,True),(13,True),(17,True),(19,True)]
isPrime :: Integer -> Bool
isPrime n
  | n <= 3 = n > 1
  | even n = False
  | otherwise = not . any (==0) . fmap (n `mod`) $ uniqueDivisors
  where
    uniqueDivisors  = [ i | i <-  [2..floor . sqrt . fromIntegral $  n] ]


-- 32.  ----------------------------------------
-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

-- >>> gcd' 36 63
-- 9
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)


-- 33.  ----------------------------------------
-- (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

-- >>> coprime 36 63
-- False

-- >>> coprime 36 61
-- True

coprime :: Integer -> Integer -> Bool
coprime a b = gcd a b == 1


-- 34.  ----------------------------------------
-- (**) Calculate Euler's totient function phi(m).

-- >>> totientPhi 10
-- 4
-- >>> totientPhi 10090
-- 4032
totientPhi :: Integer -> Integer
totientPhi 1 = 1
totientPhi n = fromIntegral . length . filter (coprime n)  $ [1..n]


-- 35.  ----------------------------------------
-- (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.


-- we test primes in [2, n/2]. If a prime is larger than half of n, the other possible
-- prime factor must be smaller n/2 which we already tested.
-- >>> primeFactors 315
-- [3,3,5,7]
--
-- >>> primeFactors 6
-- [2,3]
--
-- >>> primeFactors 2
-- []
--
-- >>> primeFactors 10090
-- [2,5,1009]
primeFactors :: Integer -> [Integer]
primeFactors n
  | isPrime n = []
  | otherwise = let primes = filter isPrime . reverse $ [2..n `div` 2]
                 in go n primes []
  where
    go 1 _ factors = factors
    go n (x:xs) factors
      | n `mod` x == 0 = go (n `div` x) (x:xs) (x:factors)
      | otherwise = go n xs factors


-- 36.  ----------------------------------------
-- (**) Determine the prime factors of a given positive integer.

-- >>> primeFactorsMult 315
-- [(3,2),(5,1),(7,1)]
--
-- >>> primeFactorsMult 10090
-- [(2,1),(5,1),(1009,1)]
primeFactorsMult :: Integer -> [(Integer, Int)]
primeFactorsMult = fmap ((,) <$> head <*> length) . group . primeFactors


-- 37.  ----------------------------------------
-- (**) Calculate Euler's totient function phi(m) (improved).

-- >>> phiImproved 10
-- 4
--
-- >>> phiImproved 16
-- 8
--
-- >>> phiImproved 10090
-- 4032
phiImproved :: Integer -> Integer
phiImproved 1 = 1
phiImproved n = foldr op 1 (primeFactorsMult n)
  where
    (p, m) `op` y = ((p - 1) * p ^ (m - 1)) * y


-- 38.  ----------------------------------------
-- (*) Compare the two methods of calculating Euler's totient function.

-- refl

-- 39.  ----------------------------------------
-- (*) A list of prime numbers.

-- >>> primeR [10, 20]
primeR :: Integer -> Integer -> [Integer]
primeR n m = filter isPrime [n..m]


-- 40.  ----------------------------------------
-- (**) Goldbach's conjecture.

-- >>> goldbach 28
-- (5,23)
-- >>> goldbach 6
-- You disproved goldbach!
goldbach :: Integer -> (Integer, Integer)
goldbach n
  | odd n || n == 2 = error "goldbach takes positive even numbers > 2 "
  | otherwise = let primes = filter isPrime $ [2..n]
                    (ls, rs) = if n < 8 then (primes, primes) else span (<= n `div` 2) primes
                    go [] _ = error "You disproved goldbach!"
                    go (x:xs) [] = go xs rs
                    go (x:xs) (y:ys)
                      | x + y == n = (x, y)
                      | otherwise = go (x:xs) ys
                 in go ls rs

-- essentially the first verioon manually wired cartesion product...
-- if you need cartesian product just use list comprehension.
goldbach' :: Integer -> (Integer, Integer)
goldbach' n = head [(x, y) | x <- primes, y <- primes, x + y == n]
  where
    primes = filter isPrime $ [2..n]


-- 41.  ----------------------------------------
-- (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.

-- >>> goldbachList 10 20
-- >>> goldbachList 10 40
goldbachList :: Integer -> Integer -> [(Integer, Integer)]
goldbachList l m = fmap goldbach . filter even $ [l..m]


{-@ Question 46 to 50 logic and codes
@-}

-- 46.  ----------------------------------------
-- Do you know conbinational logic (described by boolean) is the simplest form of
-- computational model? Output is a pure function of input.
--
-- The next more powerful model is finite automata
--
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or':: Bool -> Bool -> Bool
or' False False = False
or' _ _         = True

xor':: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _        = False

nor':: Bool -> Bool -> Bool
nor' a b = not $ or' a b

nand':: Bool -> Bool -> Bool
nand' a b = not $ and' a b

not' :: Bool -> Bool
not' True  = False
not' False = True

implies':: Bool -> Bool -> Bool
implies' a b = (not' a) `or'` b

equ':: Bool -> Bool -> Bool
equ' True True   = True
equ' False False = True
equ' _ _         = False


-- truth table
-- >>> table (\a b -> (and' a (or' a b)))
--
table :: (Bool -> Bool -> Bool) -> IO ()
table f = sequence [ putStrLn (show i ++ " " ++ show j ++ " " ++ show (f i j))
                   | i <- bs, j <- bs ] >> return ()
  where
    bs = [True, False]


-- 47.  ----------------------------------------
-- (*) Truth tables for logical expressions (2).

table2 :: (Bool -> Bool) -> IO ()
table2 f = sequence [ putStrLn (show i ++ " " ++ show (f i)) | i <- bs] >> return ()
  where
    bs = [True, False]



-- 48.  ----------------------------------------
-- (**) Truth tables for logical expressions (3).

table3 :: ([a] -> Bool) -> IO ()
table3 f = undefined



-- 49.  ----------------------------------------
-- (**) Gray codes.

gray :: Int -> [String]
gray = undefined


-- 50.  ----------------------------------------
-- (***) Huffman codes.

huffman :: [(Char, Int)] -> [(Char, Int)]
huffman = undefined

{-@ Question 54A to 60 binary trees
@-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq)

-- draw tree
instance Show a => Show (Tree a) where
  show = undefined


branch x l r = Branch x l r
leaf x = Branch x Empty Empty

-- 54A.  ---------------------------------------
-- Check whether a given term represents a binary tree

-- Not a problem in Haskell... So why list it in the question set though...?

-- 55.  ----------------------------------------
-- Construct completely balanced binary trees

-- for a complete tree, |left subtree| - |right subtree| <= 1.
--

cbalTree :: Int -> [Tree a]
cbalTree = undefined

-- 56.  ----------------------------------------
-- Symmetric binary trees

isSymmetric :: Tree a -> Bool
isSymmetric = undefined

-- 57.  ----------------------------------------
-- Binary search trees (dictionaries)

mkBinarySearchTree :: Ord a => [a] -> Tree a
mkBinarySearchTree = undefined


-- 58.  ----------------------------------------
-- Generate-and-test paradigm

symCbalTrees :: Int -> Tree a
symCbalTrees = undefined

-- 59.  ----------------------------------------
-- Construct height-balanced binary trees

hbalTree :: a -> Int -> Tree a
hbalTree = undefined

-- 60.  ----------------------------------------
-- Construct height-balanced binary trees with a given number of nodes

hbalTreeNodes :: a -> Int -> Int
hbalTreeNodes = undefined


{-@ Question 61 to 69 binary trees continued
@-}

-- 61.  ----------------------------------------
-- Count the leaves of a binary tree

countLeaves :: Tree a -> Int
countLeaves = undefined


-- 61A.  ---------------------------------------
-- Collect the leaves of a binary tree in a list

leaves :: Tree a -> [a]
leaves = undefined

-- 62.  ----------------------------------------
-- Collect the internal nodes of a binary tree in a list

internals :: Tree a -> [a]
internals = undefined


-- 62B. ----------------------------------------
-- Collect the nodes at a given level in a list

atLevel :: (Tree a) -> Int -> [a]
atLevel = undefined

-- 63.  ----------------------------------------
-- Construct a complete binary tree

completeBinaryTree :: Int -> Tree a
completeBinaryTree = undefined


-- 64.  ----------------------------------------
--  a layout algorithm for binary tree 1
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                               (Branch 'e' Empty Empty)
                                                Empty)
                                        Empty))
                        (Branch 'm' Empty Empty))
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty))
                        Empty)

layout :: Tree a -> Tree a
layout  = undefined

-- 65.  ----------------------------------------
--  another layout algorithm for binary tree 1

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                       (Branch 'g' Empty Empty)))
                        (Branch 'm' Empty Empty))
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty))
                        Empty)


layout1 :: Tree a -> Tree a
layout1  = undefined


-- 66.  ----------------------------------------
--  yet another layout algorithm for binary tree 1

layout2 :: Tree a -> Tree a
layout2  = undefined


-- 67A. ----------------------------------------
-- A string representation of binary trees

treeToString :: Tree a -> String
treeToString = undefined

stringToTree :: String -> Tree a
stringToTree = undefined


-- 68.  ----------------------------------------
-- Preorder and inorder sequences of binary trees.
-- We consider binary trees with nodes that are identified by single lower-case letters,
-- as in the example of problem P67.

preorder :: Tree a -> [a]
preorder = undefined

inorder :: Tree a -> [a]
inorder = undefined



-- 69.  ----------------------------------------
-- Dotstring representation of binary trees.



{-@ Question 70B to 73 multiway trees
@-}
data MTree a = Node a [MTree a] deriving (Eq, Show)


-- 70B. ----------------------------------------
-- (*) Check whether a given term represents a multiway tree.


-- 70C. ----------------------------------------
-- (**) Tree construction from a node string.


-- 71.  ----------------------------------------
-- (*) Determine the internal path length of a tree.


-- 72.  ----------------------------------------
-- (*) Construct the bottom-up order sequence of the tree nodes.


-- 73.  ----------------------------------------
-- (**) Lisp-like tree representation.


{-@ Question 80 to 89 graphs
@-}

-- 80.   ----------------------------------------
-- (***) Conversions


-- 81.   ----------------------------------------
-- (**) Path from one node to another one


-- 82.   ----------------------------------------
-- (*) Cycle from a given node


-- 83.   ----------------------------------------
-- (**) Construct all spanning trees


-- 84.   ----------------------------------------
-- (**) Construct the minimal spanning tree



-- 85.   ----------------------------------------
-- (**) Graph isomorphism


-- 86.   ----------------------------------------
-- (**) Node degree and graph coloration


-- 87.   ----------------------------------------
-- (**) Depth-first order graph traversal (alternative solution)


-- 88.   ----------------------------------------
-- (**) Connected components (alternative solution)


-- 89.   ----------------------------------------
-- (**) Bipartite graphs



{-@ Question 90 to 94 Miscellaneous problems
@-}
-- 90.   ----------------------------------------
-- (**) Eight queens problem


-- 91.   ----------------------------------------
-- (**) Knight's tour


-- 92.   ----------------------------------------
-- (***) Von Koch's conjecture

-- 93.   ----------------------------------------
-- (***) An arithmetic puzzle


-- 94.   ----------------------------------------
-- (***) Generate K-regular simple graphs with N nodes


{-@ Question 85 to 99 Miscellaneous continued
@-}


-- 95.   ----------------------------------------
-- (**) English number words


-- 96.   ----------------------------------------
-- (**) Syntax checker


-- 97.   ----------------------------------------
-- (**) Sudoku

-- 98.   ----------------------------------------
-- (***) Nonograms


-- 99.   ----------------------------------------
-- (***) Crossword puzzle

