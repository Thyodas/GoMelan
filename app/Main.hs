module Main (main) where

import Ast (internalEnv)
import Execution (runCode)
import File (readFileEither)
import System.Environment (getArgs, getProgName)
import System.Exit ( exitWith, ExitCode (ExitFailure))
import Prompt (replLoop)

exitOnError :: String -> Either String a -> IO a
exitOnError progName (Left errorMsg) = putStrLn (progName ++ ": " ++ errorMsg)
            >> exitWith (ExitFailure 84)
exitOnError _ (Right el) = return el

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
