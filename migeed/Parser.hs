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
lexeme = L.lexeme sc

fun = symbol "fun"
dot = symbol "."

ident = lexeme $ some letterChar

lambda = do
    fun
    name <- ident
    dot
    body <- e
    return $ Lam Tdyn name body

identifier = Vv <$> ident

number = Vi <$> (lexeme L.decimal)

true = do
    symbol "true"
    return $ Vb True

false = do
    symbol "false"
    return $ Vb False

atom = number <|> true <|> false <|> do
    symbol "("
    exp <- e
    symbol ")"
    return exp
    <|> identifier

add x = App (App (Vv "+") x)

table = [ [ InfixL $ App <$ symbol "" ],
          [ InfixL $ add <$ symbol "+" ] ]

app = makeExprParser atom table <|> atom

e = lambda <|> app

parseMigeed = do
    res <- e
    eof
    return res
