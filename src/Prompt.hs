{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Prompt
-}

module Prompt (replLoop) where

import System.IO
import System.Exit
import Ast (Env)
import Execution (runCode)
import Control.Exception (IOException, catch)

-- | Check if parentheses are close or not and return boolean
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

-- | String to print on prompt mode
promptString :: String
promptString = "GLaDOS> "

-- | Return string enter by user in prompt mode
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

-- | catch Ctrl + D when user quit prompt mode
handleEOF :: IOException -> IO String
handleEOF _ = putStrLn "\nExit..." >> exitSuccess >> pure ""

processInput :: Env -> String -> IO ()
processInput _ ":q" = putStrLn "Exit..." >> exitSuccess
processInput env input = case runCode env input of
        Left err -> putStrLn err >> replLoop env
        Right (newEnv, asts) ->
            mapM_ (putStrLn . show) asts >> replLoop newEnv

-- | Loop for prompt mode, takes env in arg
replLoop :: Env -> IO ()
replLoop env = do
    putStr promptString
    hFlush stdout
    input <- catch (getLines "") handleEOF
    processInput env input
