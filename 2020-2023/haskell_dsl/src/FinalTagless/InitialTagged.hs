module FinalTagless.InitialTagged where


{- Initial encoding: Modeling language with algebraic data type

   The result of the evalutation can be different types.
   1 + 1 gives Int, but True && True gives Bool.

   To have a evaluator that holds these types as one data type,
   we need to introduce a new sum type: Result.
-}

data Expr a
  = I Int
  | B Bool
  | Leq (Expr Int) (Expr Int)
  | And (Expr Bool) (Expr Bool)
  | Or (Expr Bool) (Expr Bool)
  | Not (Expr Bool)
  deriving (Eq, Show)

{-
   Our expression can return either Bool or Int.
   In this case constructors BR and IR are called tag of the result type.
   At runtime we can check the return type by checking the tags of the return
   value.
-}

-------------------------------------------------------------------------------
-- The TAG in the initial tagged encoding.
-------------------------------------------------------------------------------
data Result = BR Bool | IR Int

eval :: Expr a -> Result
eval (B b) = BR b
eval (I i) = IR i
eval (Leq e1 e2) = let IR i1 = eval e1; IR i2 = eval e2 in BR (i1 <= i2)
eval (And e1 e2) = let BR b1 = eval e1; BR b2 = eval e2 in BR (b1 && b2)
eval (Or e1 e2) = let BR b1 = eval e1; BR b2 = eval e2 in BR (b1 || b2)
