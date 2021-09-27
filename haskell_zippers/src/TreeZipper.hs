{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}

module TreeZipper where

import qualified Control.Monad (ap)
import           GHC.Generics  (Generic)

{-@ Zipper for tree
    Zipper for list is simple, but how does it work for a  tree?
    For a list zipper, our element is a node, and the context is the
    left hand side and the right hand size of the node. like this:
    x x x x _ x x x x
            ^
    where ^ points to our focus. This is simple because a focus can only
    have one possible predecessor and one successor.

    Now imagine a tree as an element and one hole cotext.
             a                          a
           /   \                      /   \
          b      e                   b     e
        /   \        ->        +   /   \
       c     d              c     _     d
     / ^\     \           / ^ \          \
    x    x     x         x     x          x

    where c is the current focus. How do we encode this as focus and
    context?
    c is the left child of b, and it has a sibling tree d.
    First, to get the full context, we must know the sibling d.
    And because we want to access c's children, the entire c tree should be kept.
@-}

data BinaryTree a = Leaf
                  | Branch a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Ord)

instance Show a => Show (BinaryTree a) where
  show t = ":-" <> prettyPrintTree t 0

prettyPrintTree ::  Show a => BinaryTree a -> Int -> String
prettyPrintTree Leaf _ = " _"
prettyPrintTree (Branch node left right) spaces =
  " " ++ show node ++ "\n" ++
  indent ++ ":-" ++ prettyPrintSub left ++
  indent ++ ":-" ++ prettyPrintSub right
  where
    indent = fill (spaces + 4)
    prettyPrintSub sub = prettyPrintTree sub (spaces + 4) <> "\n"
    fill = flip replicate ' '

instance Functor BinaryTree where
  fmap _ Leaf           = Leaf
  fmap f (Branch x l r) = Branch (f x) (fmap f l) (fmap f r)


-- | The breadcum type for the focus
-- for the following tree, We have TreeLeft x l as the context of b
--      x
--     / \
--    b   (l ...)
data TreeDirection a = TreeLeft a (BinaryTree a)
                     | TreeRight a (BinaryTree a)
                     deriving Show

instance Functor TreeDirection where
  fmap f (TreeLeft x t)  = TreeLeft (f x) (fmap f t)
  fmap f (TreeRight x t) = TreeLeft (f x) (fmap f t)

type TreeDirections a = [TreeDirection a]
-- you might wonder, if each tree direction only give you the parent node,
-- how can you go up further?
-- THe answer is a tree ziper doesn't only hold one tree direction, but a list of
-- it, which sort records how you end up to the current subtree.
-- If you follow the tree direction back, finally you will back to the root.
--                a
--             /    \
--            b       e
--          / 1 \      \
--        c      d      g
--             / 2 \
--            l     m
-- Take the tree above as an example. Say ou started from a, and goes left.
-- At this moment you have  (TreeZipper (tree b) [(TreeLeft a (tree e))]).
-- Now say you go right, so the focus is d you have
--  (TreeZipper (tree d) [(TreeRight b (tree c)) , (TreeLeft a (tree e))])
-- This list of tree diretions maintain the history of moveing down the tree,
-- with the context at that node. With these information you can easily go
-- back up and visit other nodes.

data TreeZipper a = TreeZipper (BinaryTree a) (TreeDirections a)
data MaybeTreeZipper a = IsTZ (TreeZipper a)
                       | NotTZ
                       deriving Show

instance (Show a) => Show (TreeZipper a) where
  show (TreeZipper t bs) = "TreeZipper" <> "\n" <> show t <> "\n" <> show bs

instance Functor TreeZipper where
  fmap f (TreeZipper t bs) =  TreeZipper (fmap f t) ((fmap . fmap) f bs)

toMaybe :: MaybeTreeZipper a -> Maybe (TreeZipper a)
toMaybe (IsTZ t) = Just t
toMaybe NotTZ    = Nothing

fromMaybe :: Maybe (TreeZipper a) -> MaybeTreeZipper a
fromMaybe (Just t) = IsTZ t
fromMaybe Nothing  = NotTZ

-- help guard operation from a normal tree zipper to maybe treezipper.
asMaybeTreeZipper :: (TreeZipper a -> MaybeTreeZipper a) -> MaybeTreeZipper a -> MaybeTreeZipper a
asMaybeTreeZipper _ NotTZ    = NotTZ
asMaybeTreeZipper f (IsTZ t) =  f t

isRoot :: TreeZipper a -> Bool
isRoot (TreeZipper _ []) =  True
isRoot _                 = False

isLeaf :: TreeZipper a -> Bool
isLeaf (TreeZipper Leaf _) = True
isLeaf _                   = False

fromTree :: BinaryTree a -> TreeZipper a
fromTree t = TreeZipper t []

toTree :: TreeZipper a -> BinaryTree a
toTree z = case toRoot z of
             IsTZ (TreeZipper t _) -> t

moveLeft :: TreeZipper a -> MaybeTreeZipper a
moveLeft (TreeZipper (Branch x l r) bs) = IsTZ $ TreeZipper l (TreeLeft x r : bs)
moveLeft (TreeZipper Leaf _) = NotTZ

moveRight :: TreeZipper a -> MaybeTreeZipper a
moveRight (TreeZipper (Branch x l r) bs) = IsTZ $ TreeZipper r (TreeRight x l : bs)
moveRight (TreeZipper Leaf _) = NotTZ

moveUp :: TreeZipper a -> MaybeTreeZipper a
moveUp (TreeZipper t ((TreeLeft x l) : bs)) = IsTZ $  TreeZipper (Branch x l t) bs
moveUp (TreeZipper _ []) = NotTZ

modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (TreeZipper (Branch x l r) bs) = TreeZipper (Branch (f x) l r) bs
modify _ (TreeZipper Leaf bs)           = TreeZipper Leaf bs

set :: a -> TreeZipper a -> TreeZipper a
set a = modify (const a)

attach :: BinaryTree a -> TreeZipper a -> TreeZipper a
attach t (TreeZipper Leaf bs) = TreeZipper t bs
attach _ z                    = z

toRoot :: TreeZipper a -> MaybeTreeZipper a
toRoot (TreeZipper t []) = IsTZ $ TreeZipper t []
toRoot z                 = case moveUp z of
                             IsTZ z' -> toRoot z'
                             NotTZ   -> IsTZ  z

tree = Branch 5
  (Branch 7
    Leaf
    (Branch 3 Leaf Leaf))
  (Branch 10
    (Branch 6 Leaf Leaf)
    Leaf)

z = fromTree tree
