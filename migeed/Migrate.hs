{-# Language QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Maximality
import Lang
import Parser
import Text.Megaparsec
import System.Environment
import qualified Data.Text.IO as T
import Data.Maybe

main = do
    args <- getArgs
    text <- T.readFile (head args)
    print tenv
    case parse parseMigeed (head args) text of
        Left error -> print error
        Right program -> print $ fromJust $ closestMaximalMigration_n program 5 tenv

