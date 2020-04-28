module Parser (parseExpr) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data
import Data.Void (Void)
import Text.Megaparsec ((<?>), Parsec, between, empty, errorBundlePretty, label, many, runParser)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseExpr :: String -> Either String Expr
parseExpr = mapLeft errorBundlePretty . runParser expr ""
  where
    mapLeft _ (Right x) = Right x
    mapLeft f (Left x) = Left $ f x

expr :: Parser Expr
expr = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation,
      prefix "+" id
    ],
    [ binary "*" Mult,
      binary "/" Div
    ],
    [ binary "+" Plus,
      binary "-" Minus
    ]
  ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

term :: Parser Expr
term = constant <|> variable <|> parens expr

constant :: Parser Expr
constant = Constant <$> number <?> "constant"

variable :: Parser Expr
variable = fmap Var . label "variable" . lexeme $ do
  c <- letterChar
  r <- many alphaNumChar
  pure $ c : r

parens :: Parser Expr -> Parser Expr
parens = fmap Parens . between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

number :: Parser Int
number = L.signed (pure ()) $ lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 empty empty
