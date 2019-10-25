module Main where

import System.Environment(getArgs)
import qualified VM
import qualified Interpreter
import qualified REPL

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--repl"] -> REPL.main
        ["-r"] -> REPL.main
        ["--interpreter"] -> Interpreter.main'
        ["-i"] -> Interpreter.main'
        _ -> Interpreter.main
