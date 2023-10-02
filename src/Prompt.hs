module Prompt where

import System.IO
import System.Exit
import Ast (Env)
import Execution (runCode)

replLoop :: Env -> IO ()
replLoop env = do
    putStr "GLaDOS> "
    hFlush stdout
    input <- getLine
    if input == ":q"
        then
            putStrLn "Exit..." >>
            exitSuccess
        else
            case runCode env input of
                Left err -> putStrLn err >> replLoop env
                Right (newEnv, asts) -> mapM_ (putStrLn . show) asts
                    >> replLoop newEnv
