module Main where

import Lib
import Runner
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        file:_ -> print =<< runFile file
        [] -> errorWithoutStackTrace "need filename"
