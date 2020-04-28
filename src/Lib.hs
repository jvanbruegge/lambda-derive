module Lib (runParenthesizer) where

import Data
import Parser (parseExpr)

runParenthesizer :: String -> Either String String
runParenthesizer = fmap (printExpr . parenthesize) . parseExpr

parenthesize :: Expr -> Expr
parenthesize (Plus a b) = Parens $ Plus (parenthesize a) (parenthesize b)
parenthesize (Minus a b) = Parens $ Minus (parenthesize a) (parenthesize b)
parenthesize (Mult a b) = Parens $ Mult (parenthesize a) (parenthesize b)
parenthesize (Div a b) = Parens $ Div (parenthesize a) (parenthesize b)
parenthesize (Negation x) = Parens $ Negation $ parenthesize x
parenthesize x = x
