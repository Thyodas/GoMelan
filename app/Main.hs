{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Main
-}

module Main (main) where

import InternalFunctions (internalEnv)
import Execution (runCode)
import File (readFileEither)
import System.Environment (getArgs)
import System.Exit ( exitWith, ExitCode (ExitFailure))
import Prompt (replLoop)

fileExecution :: String -> IO ()
fileExecution path = do
    file <- readFileEither path
    case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> case runCode internalEnv content of
            Left err -> putStrLn err >> exitWith (ExitFailure 84)
            Right (_, asts) -> mapM_ (putStrLn . show) asts

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
