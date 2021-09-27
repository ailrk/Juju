module Zipper where

{-@ Zipper
    using zipper to change the focus.
    zipper allows you to traverse the cursor along some data structure
    in a effcient way.

    Imagine a list [1, 2, 3, 4, 5], cursor on 3
    we can view it as: [1, 2] 3 [4, 5], meaning we currently
    focus on 3

    MultiwayTreeZipper is commonly used to represent tree like data like
    DOM, json. etc..

    Motivation:
    Note moving the focus is not a problem in imperative languages at all.
    Take C++ as an example, you have pointer or reference, so you can point
    to what ever element at whatever time you like. If an element is hidden
    in a data structure, once you find it you can create a pointer point to
    that element, then you can refer to the same element with constant time
    all the time.

    But it's not true for immutable data structure. If you have a
    deeply nested haskell tree, and you want to modify a leave node to some
    other value. To do that you need to find the leave by traversing to there,
    copy the entire tree, and substitue that node with modified value.

    Can you do better in haskell? Turns out not really as good as pointer,
    because that's literately the smallest cost you can get for element
    accessing. But there are something you can do to minimize the cost.
    zipper is designed to avoid a full copy of the old tree, and control the
    access time down to constant/logrhtmic time.
@-}



-- implement a list zipper.
-- note, for list [1, 2, 3, 4, 5, 6], you have
-- ListZipper [3, 2, 1] 4 [5, 6]
-- the reason why ls is reversed is that you can access from the element
-- closer to cursor from the beginning, behave like a list.
data ListZipper a = ListZipper [a] a [a] deriving (Eq, Show)

lefts :: ListZipper a -> [a]
lefts (ListZipper l _ _) = l

rights :: ListZipper a -> [a]
rights (ListZipper _ _ r) = r

-- A common pattern is to define your own maybe. This void newtype
-- wrapping and unwrapping.
-- In case if you need to use maybe related function, just provide
-- a map to maybe.
data MaybeListZipper a = IsZ (ListZipper a) | IsNotZ
  deriving (Eq, Show)

-- isZ :: ListZipper a -> MaybeListZipper a
-- isZ = MListsZipper . Just

-- isNotZ :: MaybeListZipper a
-- isNotZ = MListsZipper Nothing

instance Functor ListZipper where
  fmap f (ListZipper l x r) =
    ListZipper (fmap f l) (f x) (fmap f r)

instance Applicative ListZipper where
  pure a = ListZipper (repeat a) a (repeat a)
  (ListZipper l x r) <*> (ListZipper l' x' r') =
    ListZipper (l <*> l') (x x') (r <*> r')

-- list zipper acutally can be implemented more efficiently
-- with an finger tree or something.
toList :: ListZipper a -> [a]
toList (ListZipper l x r) = reverse l <> (x : r)

toListZ :: MaybeListZipper a -> [a]
toListZ IsNotZ  = []
toListZ (IsZ l) = toList l

fromList :: [a] -> MaybeListZipper a
fromList []     = IsNotZ
fromList (x:xs) = IsZ $ ListZipper [] x xs

toMaybe :: MaybeListZipper a -> Maybe (ListZipper a)
toMaybe IsNotZ   = Nothing
toMaybe (IsZ lz) = Just lz

-- function version of constructor. people always make this for some reasons.
-- might be a smart constructor, but since in this case it doesn't check anything
-- it's just a shorthand.
zipper :: [a] -> a -> [a] -> ListZipper a
zipper = ListZipper

fromMaybe :: Maybe (ListZipper a) -> MaybeListZipper a
fromMaybe Nothing   = IsNotZ
fromMaybe (Just ls) = IsZ ls

asZipper :: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asZipper f = asMaybeZipper (IsZ . f)

asMaybeZipper :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asMaybeZipper _ IsNotZ  = IsNotZ
asMaybeZipper f (IsZ z) = f z

-- modify the focus
withFocus :: (a -> a) -> ListZipper a -> ListZipper a
withFocus f (ListZipper l x r) = ListZipper l (f x) r

setFocus :: a -> ListZipper a -> ListZipper a
setFocus = withFocus . const

-- nice syntatic sugar. with this you can write a .= 20.
(.=) :: ListZipper a -> a -> ListZipper a
(.=) = flip setFocus

hasLeft :: ListZipper a -> Bool
hasLeft (ListZipper l _ _) = not (null l)

hasRight :: ListZipper a -> Bool
hasRight (ListZipper _ _ r) = not (null r)

-- find from the left and gives you a new zipper focus on the
-- target found.
-- Note: left part has reversed order.
findLeft :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findLeft p (ListZipper ls x rs) =
  case break p ls of
    (_, []) -> IsNotZ
    (r', x':l') ->
      IsZ (ListZipper l' x' $ reverse r' <> (x : rs))

findRight :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findRight p (ListZipper l x r) =
  case break p r of
    (_, [])     -> IsNotZ
    (l', x':r') -> IsZ (ListZipper (reverse l' <> (x : l)) x' r')

-- move the zipper left. If no element to the left then wrap.
moveLeftLoop :: ListZipper a -> ListZipper a
moveLeftLoop (ListZipper [] x r) =
  let (x', r') = foldl (\(p, q) z -> (z, p : q)) (x, []) r
   in ListZipper r' x' []
moveLeftLoop (ListZipper (l:ls) x r) = ListZipper ls l (x:r)

-- dual to moveLeftLoop
moveRightLoop :: ListZipper a -> ListZipper a
moveRightLoop (ListZipper l x []) =
  let (x', l') = foldl (\(p, q) z -> (z, p: q)) (x, []) l
   in ListZipper [] x' l'
moveRightLoop (ListZipper l x (r:rs)) = ListZipper (x:l) r rs

-- move one left.
moveLeft :: ListZipper a -> MaybeListZipper a
moveLeft (ListZipper [] _ _)     =  IsNotZ
moveLeft (ListZipper (l:ls) x r) = IsZ (ListZipper ls l (x:r))

-- move one right.
moveRight :: ListZipper a -> MaybeListZipper a
moveRight (ListZipper _ _ [])     = IsNotZ
moveRight (ListZipper l x (r:rs)) = IsZ (ListZipper (x:l) r rs)

-- swap with left element of the cursor.
swapLeft :: ListZipper a -> MaybeListZipper a
swapLeft (ListZipper [] _ _)     = IsNotZ
swapLeft (ListZipper (l:ls) x r) = IsZ $ ListZipper (x:ls) l r

-- dual of swapLeft
swapRight :: ListZipper a -> MaybeListZipper a
swapRight (ListZipper _ _ [])     = IsNotZ
swapRight (ListZipper l x (r:rs)) = IsZ $ ListZipper l r (x:rs)

dropLefts :: ListZipper a -> ListZipper a
dropLefts (ListZipper _ x r) = ListZipper [] x r

dropRights :: ListZipper a -> ListZipper a
dropRights (ListZipper l x _) = ListZipper l x []

-- move left N element. If it's negate move to the right.
moveLeftN :: Int -> ListZipper a -> MaybeListZipper a
moveLeftN n z | n == 0 = IsZ z
  | n < 0 = moveRightN (negate n) z
  | otherwise = asMaybeZipper (moveLeftN (n - 1)) (moveLeft z)

-- dual of moveLeftN
moveRightN :: Int -> ListZipper a -> MaybeListZipper a
moveRightN n z | n == 0 = IsZ z
  | n < 0 = moveLeftN (negate n) z
  | otherwise = asMaybeZipper (moveRightN (n - 1)) (moveRight z)

-- move left n elements. If can't move by n, return the maxium -- number m that the cursor can be moved.
moveLeftN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveLeftN' n z = moveLeftN'' n z 0
  where
    moveLeftN'' n' z' q | n == 0 = Right z'
      | n < 0 = moveRightN' (negate n') z'
      | otherwise = case moveLeft z' of
                      IsZ zz -> moveLeftN'' (n' - 1) zz (q + 1)
                      IsNotZ -> Left q

moveRightN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveRightN' n z = moveRightN'' n z 0
  where
    moveRightN'' n' z' q | n' == 0 = Right z
      | n' < 0 = moveLeftN' (negate n') z'
      | otherwise = case moveRight z' of
                      IsZ z'' -> moveRightN'' (n' - 1) z'' (q + 1)
                      IsNotZ  -> Left q

-- index in the zipper.
-- if i > a, move to the right by (i - a)
-- if i <= a, move  to the right
-- [_ _ _ _ _ _ _ _]
--        ^
--            i      : case i > a
--    i              : case i < a
--
nth :: Int -> ListZipper a -> MaybeListZipper a
nth i z
  | i < 0 = IsNotZ
  | otherwise = case moveLeftN' i z of
                  Left a                   -> moveRightN (i - a) z
                  Right (ListZipper l _ _) -> moveLeftN (length l) z

index :: ListZipper a -> Int
index (ListZipper l _ _) = length l

end :: ListZipper a -> ListZipper a
end z = case moveRight z of
          IsNotZ -> z
          IsZ z' -> end z'

start :: ListZipper a -> ListZipper a
start z = case moveLeft z of
            IsNotZ -> z
            IsZ z' -> start z'


{-@ Zipper vs lens
    Zipper and lens looks very similar, but there are good for
    different use cases.

    Lens allows you to compose lens together to access in nested
    data structure, but it can only access the data it "points" to,
    you can't not access the surrouding of the focus.

    Zipper, on the other hand, gives you the ability to access the
    surroudings. In the list zipper case, you can access what's before
    and what's after. Lens doesn't allow you to do that.

    Again, a simple pointer beat all of these, you can pin on one exact
    data with almost no overhead, and do arithmetic on that pointer to
    access the surroundings. Of course if it's not an array but some other
    more complicated data structures, the cost will be higher.
@-}

{-@ Algebra of zipper
    First we know algebraic data type is called algebraic because
    you can do algebra to work out the size of the type.

    - Maybe a -> Nothing | Just a -> 1 + a
    - Either Void a -> _ | Right a -> 0 + a, you can not have Left case.
    - (Void, a) -> 0 * a = 0, this type is not constructable.
      ...

    So take a look at this
    - Either x (x, x)  ->  (x + (x * x))
      d/dx(x + (x * x))  = (1 + (2 * x))
      (1 + (2 * x)) -> Maybe (Bool, x)


    - (Either x x, Either x x) -> (x + x) * (x + x)
      d/dx (x + x) * (x + x) = 8x
      8x -> Maybe bool -> (Bool, x)

    - (x, x, x) -> (x * x * x)
      d/dx (d^3) = 3x^2 -> (Maybe bool, x, x)

    Enough. the point is everytime you differetiante the datatype you
    get a thing that looks like a zipper.

@-}

-- https://www.youtube.com/watch?v=HqHdgBXOOsE
-- https://en.wikibooks.org/wiki/Haskell/Zippers#Differentiation_of_data_types
-- http://hackage.haskell.org/package/list-zipper-0.0.10/docs/src/Data.ListZipper.html#ListZipper
