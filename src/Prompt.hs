module Prompt where

import System.IO
import System.Exit
import Ast (Env)
import Execution (runCode)
import Control.Exception (IOException, catch)

areParenthesesClosed :: String -> Bool
areParenthesesClosed = areParenthesesClosed' 0
    where
        areParenthesesClosed' :: Int -> String -> Bool
        areParenthesesClosed' 0 [] = True
        areParenthesesClosed' _ [] = False
        areParenthesesClosed' n ('(':xs) = areParenthesesClosed' (n + 1) xs
        areParenthesesClosed' n (')':xs) = areParenthesesClosed' (n - 1) xs
        areParenthesesClosed' n (_:xs)
            | n < 0 = False
            | otherwise = areParenthesesClosed' n xs

promptString :: String
promptString = "GLaDOS> "

getLines :: String -> IO String
getLines str = do
    hFlush stdout
    line <- getLine
    if areParenthesesClosed (str ++ line)
        then return (str ++ line)
        else do
            putStr $ "..." ++ replicate (length promptString - 3) ' '
            nextLine <- getLines (str ++ line)
            return (nextLine)

handleEOF :: IOException -> IO String
handleEOF _ = putStrLn "\nExit..." >> exitSuccess >> pure ""

replLoop :: Env -> IO ()
replLoop env = do
    putStr promptString
    hFlush stdout
    input <- catch (getLines "") handleEOF
    if input == ":q"
        then
            putStrLn "Exit..." >>
            exitSuccess
        else
            case runCode env input of
                Left err -> putStrLn err >> replLoop env
                Right (newEnv, asts) -> mapM_ (putStrLn . show) asts
                    >> replLoop newEnv
