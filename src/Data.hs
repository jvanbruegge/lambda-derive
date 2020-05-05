module Data where

import Recursion (Fix (..), cata)

data ExprF f
  = Plus f f
  | Minus f f
  | Mult f f
  | Div f f
  | Parens f
  | Var String
  | Constant Int
  | Negation f
  deriving stock (Show, Eq, Functor)

type Expr = Fix ExprF

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

plus, minus, mult, division :: Expr -> Expr -> Expr
plus = In ... Plus
minus = In ... Minus
mult = In ... Mult
division = In ... Div

parens, negation :: Expr -> Expr
parens = In . Parens
negation = In . Negation

constant :: Int -> Expr
constant = In . Constant

var :: String -> Expr
var = In . Var

printExpr :: Expr -> String
printExpr = cata \case
  Constant n -> show n
  Var v -> v
  Parens x -> "(" <> x <> ")"
  Plus a b -> a <> " + " <> b
  Minus a b -> a <> " - " <> b
  Mult a b -> a <> " * " <> b
  Div a b -> a <> " / " <> b
  Negation x -> "-" <> x
