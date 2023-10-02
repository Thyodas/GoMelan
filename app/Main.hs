module Main (main) where

import Ast (internalEnv)
import Execution (runCode)
import File (readFileEither)
import System.Environment (getArgs, getProgName)
import System.Exit ( exitWith, ExitCode (ExitFailure))

import System.IO
import System.Exit

-- main :: IO ()
-- main = do
--     -- putStrLn "Welcom to my Haskell Prompt!"
--     putStrLn "Type ':q' to quit."
--     replLoop

replLoop :: IO ()
replLoop = do
    putStr "Prelude> "
    hFlush stdout
    input <- getLine
    if input == ":q"
        then do
            putStrLn "Exit..."
            exitSuccess
        else do
            processInput input
            replLoop

processInput :: String -> IO ()
processInput input = putStrLn $ input -- fonction who manage error


handleArgs :: IO (Either String [String])
handleArgs = do
    args <- getArgs
    if length args /= 1 then
        return $ Left "Usage: ./funEvalExpr <file>"
    else
        return $ Right args

exitOnError :: String -> Either String a -> IO a
exitOnError progName (Left errorMsg) = putStrLn (progName ++ ": " ++ errorMsg)
            >> exitWith (ExitFailure 84)
exitOnError _ (Right el) = return el

main :: IO ()
main = do
    progName <- getProgName
    args <- handleArgs
    [path] <- exitOnError progName args
    file <- readFileEither path
    case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> case runCode internalEnv content of
            Left err -> putStrLn err >> exitWith (ExitFailure 84)
            Right asts -> mapM_ (putStrLn . show) asts

