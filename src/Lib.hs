module Lib (run, parenthesize, simplify) where

import Data
import Data.Ratio (denominator, numerator)
import Debug.Trace (traceShow)
import Parser (parseExpr)
import Recursion (Fix (..), cata, para)

run :: (Expr -> Expr) -> String -> Either String Expr
run f = fmap f . parseExpr

parenthesize :: Expr -> Expr
parenthesize = cata \case
  Plus a b -> parens $ plus a b
  Minus a b -> parens $ minus a b
  Mult a b -> parens $ mult a b
  Div a b -> parens $ division a b
  Negation x -> parens $ negation x
  Power a b -> parens $ power a (if isSimple b then b else parens b)
  x -> In x

simplify :: Expr -> Expr
simplify = cata \case
  Plus (In (Constant x)) (In (Constant y)) -> constant $ x + y
  Minus (In (Constant x)) (In (Constant y)) -> constant $ x - y
  Mult (In (Constant x)) (In (Constant y)) -> constant $ x * y
  Div (In (Constant x)) (In (Constant y)) | y /= 0 -> constant $ x / y
  Negation (In (Negation x)) -> x
  Power (In (Constant x)) (In (Constant y)) | denominator y == 1 -> constant $ x ^ numerator y
  x -> In x
