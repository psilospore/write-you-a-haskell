module Chapter2Parsec where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Token (GenLanguageDef (reservedNames))
import qualified Text.Parsec.Token as Tok
import qualified Text.ParserCombinators.Parsec.Expr as Ex

-- Tokens
-- To create a lexer handle individual characters
-- comments for example are just ommited form the parse stream
-- others as identifiers or operators

-- This is some general config for a language
-- pretty cool
langDef :: Tok.LanguageDef ()
langDef =
    Tok.LanguageDef
        { Tok.commentStart = "{-"
        , Tok.commentEnd = "-}"
        , Tok.commentLine = "--"
        , Tok.nestedComments = True
        , Tok.identStart = letter
        , Tok.identLetter = alphaNum <|> oneOf "_'"
        , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
        , Tok.reservedNames = undefined
        , Tok.reservedOpNames = undefined
        , Tok.caseSensitive = True
        }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- Like semi columns? Between statements
semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

-- Reserved operators . I'm guessing like -> or :: in haskell?
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

prefixOp s f =
    Ex.Prefix (reservedOp s >> return f)
