{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module QQHtml where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Parsec
import           Text.Parsec.String


data Node = Tag String [Node]
          | Text String
          deriving Show

-- first define a normal parser
textNode :: Parser Node
textNode = Text <$> many1 (satisfy (/= '<'))

tagNode :: Parser Node
tagNode = do
  tagName <- char '<' *> many1 letter <* char '>'
  children <- many $ try tagNode <|> textNode
  string "</" *> string tagName *> char '>'
  return $ Tag tagName children

-- define how do we lift a value into Q.
instance Lift Node where
  lift (Text t)            = [| Text t |]
  lift (Tag name children) = [| Tag name children |]

-- more complicated lifting.
htmlExpr :: String -> Q Exp
htmlExpr str = do
  filename <- loc_filename <$> location
  case parse tagNode filename str of
    Left _    -> lift "error"
    Right tag -> [| tag |]

-- quasi quotes for patterns
-- [p||] is a quasi quoter for definiing pat.
htmlPat :: String -> Q Pat
htmlPat "<_>"   = [p| Tag _ children |]
htmlPat "<strong>"   = [p| Tag "strong" children |]
htmlPat "#text" = [p| Text text |]
htmlPat ('<':rest) = return $
  ConP (mkName "HTML.tag")
    [ LitP (StringL (init rest))
    , VarP (mkName "children")]

-- define quiasi quoter. ghc will recognize [html|...|] from now on that
-- equipped with htmlExpr and htmlPat.
html :: QuasiQuoter
html = QuasiQuoter
  htmlExpr
  htmlPat
  undefined
  undefined
