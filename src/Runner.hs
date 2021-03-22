module Runner where

import Parser
import Interpreter

swallow :: Show err => Either err a -> a
swallow = \case
    Left err -> errorWithoutStackTrace (show err)
    Right a -> a

swallows :: Either String p -> p
swallows = \case
    Left err -> errorWithoutStackTrace err
    Right a -> a

runString :: String -> String -> IO Result
runString name src = do
    let parsed = swallows (parseProgram name src)
    swallow <$> interpretProgram parsed

runFile :: String -> IO Result 
runFile file = do
    src <- readFile file
    runString file src
