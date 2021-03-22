{-# Language QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser where
import Lang
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void
import Data.Text (Text)
import qualified Data.Text.IO as T

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

symbol = L.symbol sc

fun = symbol "fun"
dot = symbol "."

ident = many letterChar

lambda = do
    fun
    space
    name <- ident
    space
    dot
    space
    body <- e
    space
    return $ Lam Tdyn name body

identifier = do
    name <- ident
    return $ Vv name

number = do
    n <- L.decimal
    return $ Vi n

atom = number <|> do
    char '('
    exp <- e
    char ')'
    return exp
    <|> try(identifier)

add x = App (App (Vv "+") x)

table = [ [ InfixL $ App <$ char ' ' ],
          [ InfixL $ add <$ char '+' ] ]

app = makeExprParser atom table <|> atom

e = lambda <|> app

parseMigeed = do
    res <- e
    space
    eof
    return res
