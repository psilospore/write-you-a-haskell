module Chapter2Parser where

import Chapter2AST
import Chapter2Parsec (lexer, prefixOp, reserved, reservedOp)
import qualified Chapter2Parsec as Chapter2Parsec
import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity)
import Text.Parsec (eof)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

prefixOp s f =
    Ex.Prefix $ do
        reservedOp s
        return f

-- TODO read about OperatorTable. Not sure what this is
table :: Ex.OperatorTable String () Identity Expr
table =
    [
        [ Chapter2Parser.prefixOp "succ" Succ
        , Chapter2Parser.prefixOp "pred" Pred
        , Chapter2Parser.prefixOp "iszero" IsZero
        ]
    ]

```
if True
    putStr "blah"
else
    putStr "hi"
end
```

data If = If {
    cond: Expr,
    trueStatement: Expr,
    falseStatement: Expr
}

eval IfThenElse -> Bytecode
eval = undefined

ifthen :: Parser Expr
ifthen = do
    Chapter2Parsec.reserved "if"
    cond <- expr
    Chapter2Parsec.reservedOp "then"
    trueExpr <- expr -- If true run this body
    Chapter2Parsec.reserved "else"
    falseExpr <- expr -- If false run this body
    return (If cond trueExpr falseExpr)

true, false, zero :: Parser Expr
true = do
    reserved "true"
    return Tr
false = do
    reserved "false"
    return Fl
zero = do
    reserved "0"
    return Zero

-- TODO read this
expr :: Parser Expr
expr = Ex.buildExpressionParser table factor

factor :: Parser Expr
factor =
    true
        <|> false
        <|> zero
        <|> ifthen
        <|> Chapter2Parsec.parens expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    return r

-- Values are in normal form. Cannot be reduced further

isNum Zero = True
isNum (Succ t) = isNum t
isNum _ = False
