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
import System.Environment (getProgName)
import Execution (execBuild, execRun)

data Options = Run { src :: FilePath }
    | Build { src :: FilePath, out :: FilePath }
    | Interactive { optSrc :: FilePath }
    deriving (Data, Typeable, Show, Eq)

outFlags :: FilePath -> FilePath
outFlags x = x &= help "Output file" &= typFile

srcFlags :: Int -> FilePath
srcFlags i = def &= argPos i &= typFile

run :: Options
run = Run
    {src = srcFlags 0
    }
    &= help "Run Gomelan"

build :: Options
build = Build
    {src = srcFlags 1
    ,out = outFlags "out.gomc"
    }
    &= help  "Build Gomelan"

interactive :: Options
interactive = Interactive
    {optSrc = def &= opt "" &= argPos 0 &= typ "FILE"
    }
    &= help ("Run Gomelan in interactive mode.")

myModes :: Mode (CmdArgs Options)
myModes = cmdArgsMode $ modes [build, run, interactive]
    &= versionArg [explicit, name "version", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
    &= verbosity

_PROGRAM_NAME :: String
_PROGRAM_NAME = "gomelan"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "1.0.0"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "Gomelan program able to parse and execute " ++
                "Gomelan programmation language"

_COPYRIGHT :: String
_COPYRIGHT = "(C) 2023 GIACOMEL Marie - HEIN Guillaume - HOURTOULLE Tristan -"
                ++ " PARENTEAU Thomas - ROSSIGNON Lucas"

getGomelanArgs :: IO ()
getGomelanArgs = do
    progArgs <- getArgs
    opts <- (if null progArgs then withArgs ["--help"] else id) $
        cmdArgsRun myModes
    optionHandler opts

optionHandler :: Options -> IO ()
optionHandler opts = runArgs opts

runArgs :: Options -> IO ()
runArgs opts@Build {src = src, out = out} = execBuild src out
runArgs opts@Run {src = src} = execRun src
runArgs opts@Interactive {optSrc = ""} = putStrLn
    "Interactive option default !" >> print opts
runArgs opts@Interactive {optSrc = src} = putStrLn
    "Interactive option !" >> print opts
