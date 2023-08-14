{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module HigherOrderCPS where

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



-- Higher order transformaiion. much like higher order abstract syntax we reuse
-- the host language's lambda.

-- src. just unyped lambda calculus.
data Expr = Lam Text Expr
          | Var Text
          | App Expr Expr
          deriving Show

data AExpr
  = AVar Text
  | ALam [Text] CExpr
  deriving Show


instance PP.Pretty AExpr where
 pretty aexpr = case aexpr of
    AVar v -> PP.textStrict v
    ALam vs cexpr ->
      let pvs = PP.hcat (fmap PP.pretty vs)
      in  PP.text "(\\ " <> pvs <> PP.text ". "
          PP.<$> PP.indent 2 (PP.pretty cexpr) <> PP.text ")"

instance PP.Pretty CExpr where
  pretty cexpr = case cexpr of
    CApp aexpr aexprs ->
      let paexprs = fmap PP.pretty aexprs
      in  PP.text "(" <> PP.pretty aexpr
            PP.<+> PP.fillSep paexprs <> PP.text ")"


data CExpr = CApp AExpr [AExpr] deriving Show

-------------------------------------------------------------------------------
gensym :: IO Text
gensym = fmap (\u -> T.pack ("#" <> show (hashUnique u))) newUnique

-------------------------------------------------------------------------------
-- reuse haskell's binder

hoM :: Expr -> MaybeT IO AExpr
hoM expr@(Lam var e) = do
  k1 <- liftIO gensym
  cexpr <- hoT e (\e' -> return $ CApp (AVar k1) [e'])
  return $ ALam [var, k1] cexpr
hoM (Var n) = return $ AVar n
hoM _ = MaybeT . pure $ Nothing

hoT :: Expr -> (AExpr -> MaybeT IO CExpr) -> MaybeT IO CExpr
hoT expr@(Lam _ _) k = hoM expr >>= k
hoT expr@(Var _) k = hoM expr >>= k
hoT (App f e) k = do
  e' <- liftIO gensym
  kForE <- do
    body <- k (AVar e')
    return $ ALam [e'] body
  let cexpr f' = hoT e (\e' -> return $ CApp f' [e', kForE])
  hoT f cexpr

-- >>> ex = App (Var "g") (Var "a")
-- >>> cexpr = hoT ex (\ans -> return (CApp (AVar "halt") [ans]))
-- >>> PP.pretty $ unsafePerformIO . runMaybeT $ cexpr
-- (g a (\ #18.  (halt #18)))ad m0)’ from being solved.ad m0)’ from being solved.ad m0)’ from being solved.
