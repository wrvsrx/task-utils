{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module FilterParser (
  parseFilter,
) where

import Data.Functor
import Text.Parsec
import Text.Parsec.Expr

data Predict
  = KeyValueExpr
      { name :: String
      , value :: String
      }
  | StringExpr String
  deriving (Show)

data FilterExpr
  = PredictExpr Predict
  | NotExpr FilterExpr
  | AndExpr FilterExpr FilterExpr
  | OrExpr FilterExpr FilterExpr
  deriving (Show)

parseFilter :: String -> Either ParseError [String]
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

renderFilter :: FilterExpr -> [String]
renderFilter (PredictExpr (KeyValueExpr name value)) = [name ++ ":" ++ value]
renderFilter (PredictExpr (StringExpr str)) =
  if not (null str) && head str `elem` ['+', '-']
    then [str]
    else ["description:" <> str]
renderFilter (AndExpr left right) = ["("] <> renderFilter left <> [")", "and", "("] <> renderFilter right <> [")"]
renderFilter (OrExpr left right) = ["("] <> renderFilter left <> [")", "or", "("] <> renderFilter right <> [")"]
renderFilter (NotExpr expr') = ["not", "("] <> renderFilter expr' <> [")"]

term :: Parsec String () FilterExpr
term = do
  spaces
  try (between (char '(') (char ')') (spaces *> expr <* spaces))
    <|> (PredictExpr <$> predictParser)

expr :: Parsec String () FilterExpr
expr = do
  buildExpressionParser table term
 where
  table =
    [ [Prefix (NotExpr <$ try (spaces *> char '!'))]
    , [Infix (AndExpr <$ try (spaces *> char '&')) AssocLeft, Infix (OrExpr <$ try (spaces *> char '|')) AssocLeft]
    ]

specialCharacter :: [Char]
specialCharacter = [' ', '&', '|', '(', ')', '!', ':']

escapedLetterParser :: Parsec String () Char
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
predictParser :: Parsec String () Predict
predictParser = do
  try keyValuePairParser <|> (StringExpr <$> stringParser)
 where
  rawStringParser = do
    c <- escapedLetterParser
    str <- manyTill escapedLetterParser (void (oneOf specialCharacter) <|> eof)
    return (c : str)
  quotedStringParser = do
    _ <- char '"'
    str <- many1 (escapedLetterParser <|> char ' ')
    _ <- char '"'
    return str
  stringParser = try rawStringParser <|> quotedStringParser
  keyValuePairParser = do
    name <- many1 letter
    _ <- char ':'
    value <- stringParser
    return KeyValueExpr{name = name, value = value}
