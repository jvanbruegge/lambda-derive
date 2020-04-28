module Data where

data Expr
  = Plus Expr Expr
  | Minus Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Parens Expr
  | Var String
  | Constant Int
  | Negation Expr
  deriving stock (Show, Eq)

printExpr :: Expr -> String
printExpr (Constant n) = show n
printExpr (Var v) = v
printExpr (Parens x) = "(" <> printExpr x <> ")"
printExpr (Plus a b) = printExpr a <> " + " <> printExpr b
printExpr (Minus a b) = printExpr a <> " - " <> printExpr b
printExpr (Mult a b) = printExpr a <> " * " <> printExpr b
printExpr (Div a b) = printExpr a <> " / " <> printExpr b
printExpr (Negation x) = "-" <> printExpr x
