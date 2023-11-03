{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- ExternalArgs
-}

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module ExternalArgs (getGomelanArgs) where
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Control.Monad (when)

data Options = Run   {}
    | Build   {}
    | Interactive   {}
    deriving (Data, Typeable, Show, Eq)

run :: Options
run = Run {}
    &= details  [ "Run Gomelan" ]

build :: Options
build = Build {}
    &= details  [ "Build Gomelan" ]

interactive :: Options
interactive = Interactive {}
    &= details  [ "Run Gomelan in interactive mode" ]

myModes :: Mode (CmdArgs Options)
myModes = cmdArgsMode $ modes [run, build, interactive]
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

_PROGRAM_NAME = "Gomelan"
_PROGRAM_VERSION = "1.0.0"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Gomelan programme able to parse and exectute Gomelan programmation language"
_COPYRIGHT = "(C) Giacomel Marie - Hein Guillaume - Hourtoulle Tristan - Prenteau Thomas - Rossignon Lucas 2011"

getGomelanArgs :: IO ()
getGomelanArgs = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun myModes
    optionHandler opts

optionHandler :: Options -> IO ()
optionHandler opts = exec opts

exec :: Options -> IO ()
exec opts@Run = putStrLn $ "Run option !"
exec opts@Build = putStrLn $ "Build option !"
exec opts@Interactive = putStrLn $ "Build option !"
