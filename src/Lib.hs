module Lib (runParenthesizer) where

import Data
import Parser (parseExpr)
import Recursion (Fix (..), cata)

runParenthesizer :: String -> Either String String
runParenthesizer = fmap (printExpr . parenthesize) . parseExpr

parenthesize :: Expr -> Expr
parenthesize = cata \case
  Plus a b -> parens $ plus a b
  Minus a b -> parens $ minus a b
  Mult a b -> parens $ mult a b
  Div a b -> parens $ division a b
  Negation x -> parens $ negation x
  x -> In x
