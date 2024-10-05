{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module FilterParser (
  parseFilter,
) where

import Data.Functor
import Data.Text qualified as T
import Text.Parsec
import Text.Parsec.Expr

data Predict
  = KeyValueExpr
      { name :: T.Text
      , value :: T.Text
      }
  | RawIdExpr Int
  | StringExpr T.Text
  deriving (Show)

data FilterExpr
  = PredictExpr Predict
  | NotExpr FilterExpr
  | AndExpr FilterExpr FilterExpr
  | OrExpr FilterExpr FilterExpr
  deriving (Show)

parseFilter :: T.Text -> Either ParseError [T.Text]
parseFilter s =
  let
    expr' =
      parse
        ( ((spaces <* eof) >> return Nothing) <|> (Just <$> (expr <* eof))
        )
        "fail to parse filter expr"
        s
   in
    fmap (maybe [] renderFilter) expr'

renderFilter :: FilterExpr -> [T.Text]
renderFilter (PredictExpr (KeyValueExpr name value)) = [name <> ":" <> value]
renderFilter (PredictExpr (RawIdExpr id')) = ["id:" <> T.pack (show id')]
renderFilter (PredictExpr (StringExpr str)) =
  case () of
    _ | T.length str > 0 && T.head str `elem` ['+', '-'] -> [str]
    _ -> ["description.contains:" <> str]
renderFilter (AndExpr left right) = ["("] <> renderFilter left <> [")", "and", "("] <> renderFilter right <> [")"]
renderFilter (OrExpr left right) = ["("] <> renderFilter left <> [")", "or", "("] <> renderFilter right <> [")"]
renderFilter (NotExpr expr') = ["not", "("] <> renderFilter expr' <> [")"]

term :: Parsec T.Text () FilterExpr
term = do
  spaces
  try (between (char '(') (char ')') (spaces *> expr <* spaces))
    <|> (PredictExpr <$> predictParser)

expr :: Parsec T.Text () FilterExpr
expr = do
  buildExpressionParser table term
 where
  table =
    [ [Prefix (NotExpr <$ try (spaces *> char '!'))]
    , [Infix (AndExpr <$ try (spaces *> char '&')) AssocLeft, Infix (OrExpr <$ try (spaces *> char '|')) AssocLeft]
    ]

specialCharacter :: [Char]
specialCharacter = [' ', '&', '|', '(', ')', '!', ':', '"']

escapedLetterParser :: Parsec T.Text () Char
escapedLetterParser = do
  c <- lookAhead anyChar
  case c of
    '\\' -> do
      _ <- char '\\'
      anyChar
    _ | c `notElem` specialCharacter -> anyChar
    _ -> fail "unexpected character"

-- string
-- quoted string
-- tag
-- key value pair
predictParser :: Parsec T.Text () Predict
predictParser = do
  try keyValuePairParser <|> try (RawIdExpr . read <$> many1 digit) <|> (StringExpr <$> stringParser)
 where
  rawStringParser = do
    c <- escapedLetterParser
    str <- manyTill escapedLetterParser (void (lookAhead (oneOf specialCharacter)) <|> eof)
    return (T.pack (c : str))
  quotedStringParser = do
    _ <- char '"'
    str <- many1 (escapedLetterParser <|> char ' ')
    _ <- char '"'
    return (T.pack str)
  stringParser = try rawStringParser <|> quotedStringParser
  keyValuePairParser = do
    name <- many1 (letter <|> char '.')
    _ <- char ':'
    value <- stringParser
    return KeyValueExpr{name = T.pack name, value = value}
