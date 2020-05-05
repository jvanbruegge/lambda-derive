module Data where

import Data.Ratio (denominator, numerator)
import Recursion (Fix (..), cata)

data ExprF f
  = Plus f f
  | Minus f f
  | Mult f f
  | Div f f
  | Parens f
  | Var String
  | Constant Rational
  | Power f f
  | Negation f
  deriving stock (Show, Eq, Functor)

type Expr = Fix ExprF

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

isConstant :: Expr -> Bool
isConstant (In (Constant _)) = True
isConstant _ = False

isSimple :: Expr -> Bool
isSimple (In (Var _)) = True
isSimple (In (Constant _)) = True
isSimple (In (Parens _)) = True
isSimple _ = False

plus, minus, mult, division, power :: Expr -> Expr -> Expr
plus = In ... Plus
minus = In ... Minus
mult = In ... Mult
division = In ... Div
power = In ... Power

parens, negation :: Expr -> Expr
parens = In . Parens
negation = In . Negation

constant :: Rational -> Expr
constant = In . Constant

var :: String -> Expr
var = In . Var

instance Show (Fix ExprF) where
  show = cata \case
    Constant x -> "Constant{" <> show x <> "}"
    Var v -> "Var{" <> v <> "}"
    Parens x -> "Parens{" <> show x <> "}"
    Plus a b -> "Plus{" <> a <> ", " <> b <> "}"
    Minus a b -> "Minus{" <> a <> ", " <> b <> "}"
    Mult a b -> "Mult{" <> a <> ", " <> b <> "}"
    Div a b -> "Div{" <> a <> ", " <> b <> "}"
    Negation x -> "Negation{" <> x <> "}"
    Power a b -> "Power{" <> a <> ", " <> b <> "}"

printExpr :: Expr -> String
printExpr = cata \case
  Constant x -> if denominator x == 1 then show (numerator x) else show (numerator x) <> "/" <> show (denominator x)
  Var v -> v
  Parens x -> "(" <> x <> ")"
  Plus a b -> a <> " + " <> b
  Minus a b -> a <> " - " <> b
  Mult a b -> a <> " * " <> b
  Div a b -> a <> " / " <> b
  Negation x -> "-" <> x
  Power a b -> a <> "^" <> b
