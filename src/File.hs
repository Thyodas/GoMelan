{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- File
-}

module File (readFileEither) where

import Control.Exception (IOException, catch)

readFileEither :: String -> IO (Either String String)
readFileEither path = (Right <$> readFile path) `catch` handleIOError
    where
        handleIOError :: IOException -> IO (Either String a)
        handleIOError e = return $ Left $ "Error reading file: " ++ show e
