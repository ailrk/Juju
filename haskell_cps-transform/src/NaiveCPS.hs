{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module NaiveCPS where

-- Naive cps converter
import           Control.Monad.IO.Class
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Maybe
import           Data.Char                    (isDigit, isLetter, isSpace)
import           Data.Functor.Identity        (Identity (Identity))
import           Data.Monoid
import           Data.Text                    as T
import           Data.Unique
import           Debug.Trace
import           GHC.IO.Unsafe
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.PrettyPrint.Leijen.Text as PP


-- How should the transfromed code look like?
-- 1. single variable x
--    x => k x
-- 2. single lambda
--    \x.x => \x k. k x
-- 3. simple application
--    (\x.x) e => (\f'. (\e'. f' e' k) e) (\x k1. k1 x)
-- 4. complex e.g
--    (\x. \y. x) z m
-- => (\f'. (\z'.
--      f' z' (\g'. (\m'.
--              g' m' k) m) z)
--    (\x k1. k1 (\y k2. k2 x))
--
-- Note: The order of evaluation is clear, f is applied to z first, then
--       the result is passed to it's continuation \g'...
-- 5. complex e.g 2
--    (\x. \y. x y) (\z. z) ((\m. m n) n)
--  first convert each components
--    -1 (\x k1. k1 (\y k2. k2 x))
--    -2 (\z k3. k3 z)
--    -3 ()
--

-- NOTE:
--  1. for simple value we just pass it to the continuation k
--  2. for application like (f e), we need to cps transfrom f and
--     e respectively to f', e'.
--     Original application becomes f' e' k where k is the continuation
--     of the entire application.

-- src. just unyped lambda calculus.
data Expr = Lam Text Expr
          | Var Text
          | App Expr Expr
          deriving Show

-- target cps
-- cps has two kinds of experssions: atomic and complex.
-- atomic expr always pure and reduce to a value
-- complex expr may not terminate, or may have side effects.

-- variable and abstraction
data AExpr
  = AVar Text
  | ALam [Text] CExpr
  deriving Show

-- application
data CExpr = CApp AExpr [AExpr] deriving Show

instance PP.Pretty AExpr where
 pretty aexpr = case aexpr of
    AVar v -> PP.textStrict v
    ALam vs cexpr ->
      let pvs = PP.hcat (fmap PP.pretty vs)
      in  PP.text "(\\" <> pvs <> PP.text ". "
          PP.<$> PP.indent 2 (PP.pretty cexpr) <> PP.text ")"

instance PP.Pretty CExpr where
  pretty cexpr = case cexpr of
    CApp aexpr aexprs ->
      let paexprs = fmap PP.pretty aexprs
      in  PP.text "(" <> PP.pretty aexpr
            PP.<+> PP.fillSep paexprs <> PP.text ")"

-------------------------------------------------------------------------------
gensym :: IO Text
gensym = fmap (\u -> T.pack ("#" <> show (hashUnique u))) newUnique

-------------------------------------------------------------------------------
-- Naive cps transformation
-- M only convert lambda into AExpr

transfromM :: Expr -> MaybeT IO AExpr
transfromM expr@(Lam var body) = do
  k' <- liftIO gensym
  cexpr' <- transfromT body (AVar k')
  return $ ALam [var, k'] cexpr'
transfromM expr@(Var var) = return $ AVar var
transfromM _ = MaybeT (return Nothing)  -- you can't lift Nothing...

-- the actual cps transform
-- simple cases like Lam and Var we just call k with them.
-- for Application we introduce two extra cps. Once for the argument and
-- once for the body
transfromT :: Expr -> AExpr -> MaybeT IO CExpr
transfromT expr k =
  case expr of
    Lam _ _ -> (CApp k) <$> (transfromM expr >>= \n -> return [n])
    Var _ ->(CApp k) <$> (transfromM expr >>= \n -> return [n])
    App f e -> do
      f' <- liftIO gensym
      e' <- liftIO gensym
      let kForE = ALam [e'] (CApp (AVar f') [AVar e', k])
      cexpr <- transfromT e kForE
      let kForF = ALam [f'] cexpr
      transfromT f kForF

-------------------------------------------------------------------------------
-- >>> ex = App (Var "g") (Var "a")
-- >>> PP.pretty <$> unsafePerformIO . runMaybeT $ transfromT ex (AVar "halt")
-- ((\ #4.
--  ((\ #5.
--    (#4 #5 halt)) a)) g)

-- native cps transformation creates many extra redexes that are not necessary
-- it's clear that this expression can be optmized into (g a halt) alone.

-- >>> ex = Lam "x" (Var "x")
-- >>> PP.pretty <$> unsafePerformIO . runMaybeT $ transfromT ex (AVar "k")
-- (k (\x#17.  (#17 x)))

-- >>> ex =  App (App (Lam "x" (Lam "y" (Var "x"))) (Var "z")) (Var "m")
-- >>> PP.pretty <$> unsafePerformIO . runMaybeT $ transfromT ex (AVar "k")
-- ((\#23.  ((\#24.
--     (#23 #24 (\#21.
--       ((\#22.
--         (#21 #22
--         k)) m)))) z))
--  (\x #25.  (#25 (\y #26.  (#26 x)))))
