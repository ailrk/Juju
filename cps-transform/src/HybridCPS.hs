{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module CPSTransform.HybridCPS where

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


data Expr = Lam Text Expr
          | Var Text
          | App Expr Expr
          deriving Show

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
