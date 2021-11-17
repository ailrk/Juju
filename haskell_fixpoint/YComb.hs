module YComb where

-------------------------------------------------------------------------------
fix :: (t -> t) -> t
fix f =
  let x = f x
   in x

-------------------------------------------------------------------------------
-- Convert a normal recursive function into fix point style
-- we facter out the recursive part and replace with a function rec.
-- then feed the recursion operator with `fix`.

fact = fix f where f del n | n == 0 = 1 | otherwise = n * del (n -1)

factorial :: Integer -> Integer
factorial = fix factorial'
  where
    factorial' :: (Integer -> Integer) -> Integer -> Integer
    factorial' del n | n <= 1 = 1 | otherwise = n * (del (n - 1))

-- infinite
sinFix = fix sin'
  where
    sin' :: (Eq t, Floating t) => (t -> t) -> t -> t
    sin' del x = if x == sin x then x else del (sin x)

-- with IO action.
printFix :: Integer -> IO Integer
printFix = fix print'
  where
    print' :: (Ord b, Num b, Show b) => (b -> IO b) -> b -> IO b
    print' del x | x >= 0 = do print x; del (x - 1) | otherwise = pure x

many_ones = fix (1:)
onst_still_const = fix (const "hello")

fib = fix
    $ \del ->
      \n   ->
        if n == 0 then 0 else if n == 1 then 1 else del (n -  1) + del (n - 2)

-- Y can be thought of as a searching algorithm, looking for the fix point
-- of it's argument -- a functional, and the fix point will be of the type
-- that the functioanl return.

-- fix point   : forall (x, f(a)), fix point of f is x such that x = f(x)
--             | note: simple algebra; f(x) = x * 2 - 2, x = 2 where is just
--                     the solution.
-- functional  : a higher order fn that takes a fn as input
-- Y combinator: a search function to find the fix point of a functional.
--             | note: fix point of a functional is another function
--             | note: by definition of fix point, Y(f) = f(Y(f)).
--
-- what's good : Y combinator is another way to express recursion without
--               the need to introduce name.

-- make Y comb : Call By Value : Y = \f. (\x. f (x x)) (\x. f (x x))
--             | Call By name  : Y f = let x = f x in x

