{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module NanoParsec where

import Control.Applicative
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
  fmap f (Parser parseA) = Parser $ \s -> (\(a, rest) -> (f a, rest)) <$> parseA s

instance Applicative Parser where
  pure = unit
  Parser parseF <*> Parser parseA = Parser (\s -> [(f a, rest2) | (f, rest1) <- parseF s, (a, rest2) <- parseA rest1])

instance Monad Parser where
  return = unit
  (>>=) = bind

-- Halts reading the stream and returns an empty stream
failure :: Parser a
failure = Parser $ \s -> []

-- Use both parsers and return both results Parser with list of size >2
combine :: Parser a -> Parser a -> Parser a
combine (Parser parsea1) (Parser parsea2) = Parser $ \s -> parsea1 s ++ parsea2 s

option :: Parser a -> Parser a -> Parser a
option (Parser parseL) (Parser parseR) = Parser $ \input ->
  case (parseL input) of
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
  some_v = ((\h t -> h : t) <$> v) <*> many_v

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

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (\c -> elem c s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl (Parser pa) (Parser pfaa) a =
  let x = pa a
   in _

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 (Parser f) (Parser faa) = _

char :: Char -> Parser Char
char c = satisfy (c ==)
