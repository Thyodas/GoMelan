{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Main
-}

module Main (main) where
import ExternalArgs (getGomelanArgs)

main :: IO ()
main = do
    getGomelanArgs
    -- args <- getArgs
    -- [path] <- exitOnError progName args
    -- startExecution args
