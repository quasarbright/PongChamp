module Main where

import Runner
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        file:_ -> runFile file
        [] -> errorWithoutStackTrace "need filename"
