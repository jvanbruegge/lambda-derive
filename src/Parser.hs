module Parser (parseExpr) where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data
import Data.Void (Void)
import Text.Megaparsec ((<?>), Parsec, between, empty, eof, errorBundlePretty, label, many, runParser)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parseExpr :: String -> Either String Expr
parseExpr = mapLeft errorBundlePretty . runParser (pExpr <* eof) ""
  where
    mapLeft _ (Right x) = Right x
    mapLeft f (Left x) = Left $ f x

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ pPrefix "-" negation,
      pPrefix "+" id
    ],
    [ pBinary "^" power
    ],
    [ pBinary "*" mult,
      pBinary "/" division
    ],
    [ pBinary "+" plus,
      pBinary "-" minus
    ]
  ]

pBinary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
pBinary name f = InfixL (f <$ symbol name)

pPrefix :: String -> (Expr -> Expr) -> Operator Parser Expr
pPrefix name f = Prefix (f <$ symbol name)

pTerm :: Parser Expr
pTerm = pConstant <|> pVariable <|> pParens pExpr

pConstant :: Parser Expr
pConstant = constant <$> number <?> "constant"

pVariable :: Parser Expr
pVariable = fmap var . label "variable" . lexeme $ do
  c <- letterChar
  r <- many alphaNumChar
  pure $ c : r

pParens :: Parser Expr -> Parser Expr
pParens = fmap parens . between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

number :: Parser Rational
number = fromInteger <$> (L.signed (pure ()) $ lexeme L.decimal)

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 empty empty
