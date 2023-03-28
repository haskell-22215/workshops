module ParserCombinators where

import Text.Parsec



type Parser a = Parsec String () a

parse_ :: Parser a -> String -> Either ParseError a
parse_ p = parse p ""



data Expr
  = Const Integer
  | Plus Expr Expr
  | Minus Expr Expr
  deriving (Show)


-- Grammar #1
-- Expr -> Const | Binary
-- Binary -> Expr + Expr | Expr - Expr

expr :: Parser Expr
expr = binaryExpr <|> constExpr                              -- <|> isn't commutative

constExpr :: Parser Expr
constExpr = (Const . read) <$> many1 digit

binaryExpr :: Parser Expr
binaryExpr = bx '+' Plus <|> bx '-' Minus
  where bx :: Char -> (Expr -> Expr -> Expr) -> Parser Expr
        bx c g = try $ do                                    -- try for lookahead
          l <- expr                                          -- showstopper
          _ <- char c
          r <- expr
          return $ g l r
