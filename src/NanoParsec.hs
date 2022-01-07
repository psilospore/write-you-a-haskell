{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module NanoParsec where

import Control.Applicative ( Alternative(empty, (<|>)) )
import Control.Monad
import Data.Char

-- A parser is a function that takes an input stream of chars
-- yeilds a parse tree by applying some parser logic over it
-- lexemes (sections of charter streams) and build up a data structure
-- for the AST
--
newtype Parser a = Parser {parse :: String -> [(a, String)]}

-- Traverse the stream of chars
-- yield a value of type a (usually the AST)
-- or fail with a parse error due to malformed inp7ut, or not consume the enitre stream
-- TODO track position info of failures for error reporting
runParser :: Parser a -> String -> a
runParser (Parser parse) s = case parse s of
  [(res, [])] -> res
  [(_, rest)] -> error $ "Parser did not consume entire stream " <> show rest
  _ -> error "blah" -- Why is it a list of tuples

-- Advance parser by consuming a single char
-- Return tuple and rest of string
item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c : rest) -> [(c, rest)]

-- Take one parse operation
-- compose it over the result of the second parse function
-- Since parse gives a list of tuples
-- A second parser function maps over hte resulting list
-- and concats the resulting nested list of lists into a single flat list
-- I guess like the monad instance for list?
--
bind' :: Parser a -> (a -> Parser b) -> Parser b
bind' (Parser parseA) f = Parser $
  \s -> concat $ (\(a, rest) -> parse (f a) rest) <$> parseA s

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser parseA) f = Parser $ \input -> concatMap (\(a, rest) -> parse (f a) rest) $ parseA input

-- Don't read from parse string
unit :: a -> Parser a
unit a = Parser $ \input -> [(a, input)]

instance Functor Parser where
  fmap f (Parser parseA) = Parser $ (fmap (\(a, rest) -> (f a, rest)) . parseA)

instance Applicative Parser where
  pure = unit
  Parser parseF <*> Parser parseA = Parser (\s -> [(f a, rest2) | (f, rest1) <- parseF s, (a, rest2) <- parseA rest1])

instance Monad Parser where
  return = unit
  (>>=) = bind

-- Halts reading the stream and returns an empty stream
failure :: Parser a
failure = Parser $ const []

-- Use both parsers and return both results Parser with list of size >2
combine :: Parser a -> Parser a -> Parser a
combine (Parser parsea1) (Parser parsea2) = Parser $ \s -> parsea1 s ++ parsea2 s

option :: Parser a -> Parser a -> Parser a
option (Parser parseL) (Parser parseR) = Parser $ \input ->
  case parseL input of
    [] -> parseR input -- Failure case? So use other parser
    res -> res

instance Alternative Parser where
  empty = failure
  (<|>) = option

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

-- | One or more.
some :: Alternative f => f a -> f [a]
some v = some_v
 where
  many_v = some_v <|> pure []
  some_v = (\h t -> h : t) <$> v <*> many_v

-- | Zero or more.
many :: Alternative f => f a -> f [a]
many v = many_v
 where
  many_v = some_v <|> pure []
  some_v = (:) <$> v <*> many_v

-- Check if matches predicate. Is a char, number, etc
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = bind item \char ->
  if p char
    then unit char -- True so return char
    else failure -- False fail

-- End of core of parser combinator
-- Additional functions defined on top of that logic

oneOf :: String -> Parser Char
oneOf s = satisfy (`elem` s)

-- Parse 1+ occurances of a
-- seperated by pfaa
-- recurse value until failue
-- chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
--chainl (Parser pa) (Parser pfaa) a = chainlHelp pa pfaa <|> return a

--chainlHelp :: Parser a -> Parser (a -> a -> a) -> Parser a
--chainlHelp (Parser pa) (Parser pfaa) = _
--
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p; rest a
 where
  rest a =
    ( do
        f <- op
        b <- p
        rest (f a b)
    )
      <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

"{bob: "alice"}"

"\{\"name\": \"alice1\"\}"

{name: "alice1"}

string :: String -> Parser String
string [] = return []
string (c : cs) = do
  char c
  string cs
  return (c : cs)

-- There's a token followed by spaces
token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

spaces :: Parser String
spaces = many $ oneOf "\n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  sign <- string "-" <|> return []
  num <- some digit
  return $ read (sign ++ num)

parens :: Parser a -> Parser a
parens innerParser = do
  reserved "("
  inner <- innerParser
  reserved ")"
  return inner

reserved :: String -> Parser String
reserved s = token (string s)


-- Calculator Grammar Section --

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving (Show)

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n -> n

int :: Parser Expr
int = do
  Lit <$> number

-- 1 + 1, 1 - 1
expr :: Parser Expr
expr = term `chainl1` addop

--
term :: Parser Expr
term = chainl1 factor mulop

-- A literal number or
factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x op = do
  reserved x
  return op

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a
