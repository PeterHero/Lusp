{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Bifunctor
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.IO (hPrint, hPutStr, hPutStrLn, stderr)

data LspMessage = LspMessage Int Object deriving (Show)

data Object
  = ObjectO [(String, Object)]
  | ArrayO [Object]
  | StringO String
  | NumberO Int
  | BoolO Bool
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
object = parseObject <|> parseArray <|> parseString <|> parseNumber <|> parseBool
  where
    parseObject = ObjectO <$> (char '{' *> sepBy parseKeyValue (char ',') <* char '}')
    parseKeyValue = (,) <$> string' <*> (char ':' *> object)
    parseArray = ArrayO <$> (char '[' *> sepBy object (char ',') <* char ']')
    parseString = StringO <$> string'
    string' = char '"' *> many (satisfy (/= '"')) <* char '"'
    parseNumber = NumberO <$> number
    parseBool = BoolO <$> ((True <$ string "true") <|> (False <$ string "false"))

-- JSON Printing
showLspMessage :: Object -> String
showLspMessage obj = "Content-Length: " ++ show (length objString) ++ "\n\n" ++ objString
  where
    objString = objString' obj
    objString' (ObjectO o) = "{" ++ showBySep (map showKeyValue o) "," ++ "}"
    objString' (ArrayO a) = "[" ++ showBySep (map show a) "," ++ "]"
    objString' (StringO s) = "\"" ++ s ++ "\""
    objString' (NumberO n) = show n
    objString' (BoolO b) = if b then "true" else "false"
    showKeyValue (a, b) = objString' (StringO a) ++ ":" ++ objString' b
    showBySep [] _ = ""
    showBySep [x] _ = x
    showBySep (x : xs) sep = x ++ sep ++ showBySep xs sep

-- Tokens
data Tok = TInt Int | TNLine | TBlanks Int | TFn | TIndent | TDedent | TIdent | TIf | TElse | TLPar | TRPar

-- JSONRpc call methods
initCall :: State -> [(String, Object)] -> Result
initCall state _ = Right $ (,) state $ ObjectO [("capabilities", ObjectO [("textDocumentSync", ObjectO [("openClose", BoolO True), ("change", NumberO 1)])])]

didOpen :: State -> [(String, Object)] -> Result
didOpen state msg = case lookup "params" msg of
  Just (ObjectO [("textDocument", ObjectO [("uri", StringO uri), ("languageId", _), ("version", _), ("text", StringO text)])]) -> Right $ (,) state {documents = set uri text (documents state)} $ ObjectO []
  _ -> Left "textDocument/didOpen received wrong parameters"

didChange :: State -> [(String, Object)] -> Result
didChange state msg = case lookup "params" msg of
  Just (ObjectO [("textDocument", ObjectO [("uri", StringO uri), ("version", _)]), ("contentChanges", ArrayO [ObjectO [("text", StringO text)]])]) -> Right $ (,) state {documents = set uri text (documents state)} $ ObjectO []
  _ -> Left "textDocument/didChange received wrong parameters"

didClose :: State -> [(String, Object)] -> Result
didClose state msg = case lookup "params" msg of
  Just (ObjectO [("textDocument", ObjectO [("uri", StringO uri)])]) -> Right $ (,) state {documents = rmv uri (documents state)} $ ObjectO []
  _ -> Left "textDocument/didClose received wrong parameters"

okO :: [(String, Object)] -> Object -> Object
okO msg obj = ObjectO [("jsonrpc", StringO "2.0"), ("id", idO), ("result", obj)]
  where
    idO = fromMaybe (NumberO 0) $ lookup "id" msg

errO :: String -> Object
errO err = ObjectO [("error", StringO err)]

-- Methods switch
type Result = Either String (State, Object)

retToMsg :: State -> [(String, Object)] -> (State -> [(String, Object)] -> Result) -> (State, Object)
retToMsg state msg f = case f state msg of
  Right (newState, result) -> (newState, okO msg result)
  Left err -> (state, errO err)

switch :: State -> Object -> (State, Object)
switch state msg = case msg of
  ObjectO obj -> case lookup "method" obj of
    Just (StringO "initialize") -> retToMsg state obj initCall
    Just (StringO "textDocument/didOpen") -> retToMsg state obj didOpen
    Just (StringO "textDocument/didChange") -> retToMsg state obj didChange
    Just (StringO "textDocument/didClose") -> retToMsg state obj didClose
    Just (StringO m) -> (state, errO $ "Called unknown method: " ++ m)
    Just _ -> (state, errO "method value must be string")
    Nothing -> (state, errO "No method key")
  _ -> (state, errO "Msg must be object")

set :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
set k v = ((k, v) :) . filter ((/= k) . fst)

rmv :: (Eq k) => k -> [(k, v)] -> [(k, v)]
rmv k = filter ((/= k) . fst)

newtype State = State
  { documents :: [(String, String)]
  }
  deriving (Show)

main :: IO ()
main = loop (State [])

loop :: State -> IO ()
loop state = do
  l1 <- getLine
  l2 <- getLine
  l3 <- getLine
  let msg = runParser parseLspMessage (unlines [l1, l2, l3])
  case msg of
    Just (LspMessage _ request, _) -> do
      let (newState, response) = switch state request
      hPutStr stderr "[stderr]: "
      hPrint stderr newState
      case response of
        ObjectO [_, _, ("response", ObjectO [])] -> hPutStrLn stderr "[stderr]: No response"
        _ -> putStrLn $ showLspMessage response
      loop newState
    Nothing -> print "Parsing error"
