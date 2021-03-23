import Text.ParserCombinators.Parsec
import System.Environment
import Data.Functor

-- This parser handles Scheme mathematical expressions.

-- The mathematical operator of a Scheme math expression.
type Op = Char

data SchemeExp =
    ScmNumber Integer      -- e.g.: 42
  | MathExp Op [SchemeExp] -- e.g.: (+ 1 2 (* 3 4) 5)
  deriving Show

-- A scheme file is made of many scheme lists
schemeFile :: GenParser Char st [SchemeExp]
schemeFile = do
  exps <- many1 schemeList
  eof
  return exps

eol :: GenParser Char st String
eol = try (string "\n\r")
       <|> string "\n"
       <?> "end of line"

-- A scheme list must begin and end with parens.
-- Optionally, there may be an end of line after
-- the close paren.

-- The first element of the list must be a mathematical
-- operator, from the set of '+', '-', '*', '/'.
-- The remaining elements should be either numbers
-- or embedded scheme lists, separated by spaces.

arithOps :: GenParser Char st Char
arithOps = do
  parseMB
  oneOf "+-*/"

schemeList :: GenParser Char st SchemeExp
schemeList = do
  parseMB
  do
    parseExpr <|> parseNum

parseExpr :: GenParser Char st SchemeExp
parseExpr = do
  char '('
  op <- arithOps
  elem <- many1 (parseNum <|> schemeList)
  char ')'
  parseMB
  return (MathExp op elem)

parseNum :: GenParser Char st SchemeExp
parseNum = do
  num <- read <$> (plus <|> minus <|> number)
  parseMB
  return (ScmNumber num)
  where
    plus = char '+' *> number
    minus = (:) <$> char '-' <*> number
    number = many1 digit

-- Parsing Mid breaks within the file (In terms of spaces or line breaks)

parseMB :: GenParser Char st ()
parseMB = spaces <|> parseEOL

parseEOL :: GenParser Char st ()
parseEOL = eol $> ()

parseScheme :: String -> Either ParseError [SchemeExp]
parseScheme = parse schemeFile "(unknown)"

main = do
  args <- getArgs
  p <- parseFromFile schemeFile (head args)
  case p of
    Left err  -> print err
    Right scm -> print scm

