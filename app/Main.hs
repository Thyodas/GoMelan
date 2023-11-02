{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Main
-}

module Main (main) where

import Parser (parseCodeToGomExpr, runParser, printErrors)
import Ast (GomAST(..), Env, EvalResult(..), EvalError(..), gomExprToGomAST)
import InternalFunctions (internalEnv)
import Execution (runCode, convertListToAST)
import File (readFileEither)
import System.Environment (getArgs)
import System.Exit ( exitWith, ExitCode (ExitFailure))
import Prompt (replLoop)

fileExecution :: String -> IO ()
fileExecution path = do
    file <- readFileEither path
    case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> case runParser parseCodeToGomExpr content of
            Left err -> putStr (printErrors content err) >> exitWith (ExitFailure 84)
            Right (out, _) -> case convertListToAST [] out of
                EvalResult (Left (EvalError msg _)) -> putStrLn msg >> exitWith (ExitFailure 84)
                EvalResult (Right (newEnv, asts)) -> print newEnv
                    >> print asts

replExecution :: IO ()
replExecution = putStrLn "Welcome to GLaDOS!" >> replLoop internalEnv

startExecution :: [String] -> IO ()
startExecution [path] = fileExecution path
startExecution [] = replExecution
startExecution _ = putStrLn "Usage: ./funEvalExpr [file]"
    >> exitWith (ExitFailure 84)

main :: IO ()
main = do
    args <- getArgs
    -- [path] <- exitOnError progName args
    startExecution args
