{-# Language QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Maximality
import Lang
import Parser
import Text.Megaparsec
import System.Environment
import qualified Data.Text.IO as T

main = do
    args <- getArgs
    text <- T.readFile (head args)
    case parse parseMigeed (head args) text of
        Left error -> print error
        Right program -> print $ closestMaximalMigration program tenv

