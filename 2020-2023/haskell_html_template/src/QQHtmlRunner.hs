{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module QQHtmlRunner where

import           QQHtml

{-
    Using the quasi quoters from QQHtml.
    process:
      1. html :: QuasiQuoter
      2. html run meta function htmlExpr :: String -> Q Exp
      3. the html get dumped into htmlExpr
      4. htmlExpr call tagNode, which is our parser.
      5. the parser parse the html string, and yields an html AST.
      6. the html ast then get lifted over and get a Q Exp
      7. the met function evaluate, the parsed html ast get shoved in call site.
-}

-- use String -> Q Expr
doc :: Node
doc = [html|<html>Hello, <strong>Template haskell</strong>! </html>|]

-- use String -> Q Pat
markdown [html|<strong>|] = "**" ++ concatMap markdown children ++ "**"
markdown [html|<_>|]   = concatMap markdown children
markdown [html|#text|] = text

run = print . markdown $ doc
