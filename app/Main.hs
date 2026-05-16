{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Control.Monad (forever)
import Data.Bifunctor
import Data.Char (isDigit)

data LspMessage = LspMessage Int Object deriving (Show)

data Object
  = ObjectO [(String, Object)]
  | ArrayO [Object]
  | StringO String
  | NumberO Int
  deriving (Show)

-- Parser definition
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> fmap (first f) (p s)

instance Applicative Parser where
  pure a = Parser $ \s -> Just (a, s)
  (Parser pf) <*> (Parser pa) = Parser $ \s ->
    case pf s of
      Just (f, s') -> fmap (first f) (pa s')
      Nothing -> Nothing

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \s ->
    case p1 s of
      ok@(Just _) -> ok
      Nothing -> p2 s

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \s -> do
    (a, rest) <- pa s
    runParser (f a) rest

-- Small parsers
item :: Parser Char
item = Parser $ \case
  [] -> Nothing
  (c : cs) -> Just (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  c <- item
  if f c then pure c else empty

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = read <$> some digit

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = foldr (\c -> (<*>) ((:) <$> char c)) (pure [])

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa ps = ((:) <$> pa <*> many (ps *> pa)) <|> pure []

-- JSON Parsing
parseLspMessage :: Parser LspMessage
parseLspMessage = LspMessage <$> len <* char '\n' <* char '\n' <*> object
  where
    len = string "Content-Length: " *> number

object :: Parser Object
object = parseObject <|> parseArray <|> parseString <|> parseNumber
  where
    parseObject = ObjectO <$> (char '{' *> sepBy parseKeyValue (char ',') <* char '}')
    parseKeyValue = (,) <$> string' <*> (char ':' *> object)
    parseArray = ArrayO <$> (char '[' *> sepBy object (char ',') <* char ']')
    parseString = StringO <$> string'
    string' = char '"' *> many (satisfy (/= '"')) <* char '"'
    parseNumber = NumberO <$> number

main :: IO ()
main = forever $ do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  let msg = runParser parseLspMessage (unlines [l1, l2, l3])
  print msg
