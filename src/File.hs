module File (readFileEither) where

import Control.Exception (IOException, catch)

readFileEither :: String -> IO (Either String String)
readFileEither path = (Right <$> readFile path) `catch` handleIOError
    where
        handleIOError :: IOException -> IO (Either String a)
        handleIOError e = return $ Left $ "Error reading file: " ++ show e
