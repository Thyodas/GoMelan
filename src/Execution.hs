{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Execution
-}

module Execution (runCode, convertListToAST, execBuild, execRun) where

import Parser (ErrorMsg, parseCodeToGomExpr, Parser(..),
    printErrors)
import Ast (GomAST(..), EvalResult (..), gomExprToGomAST,
    EvalError(..), Env, GomExpr(..))
import VirtualMachine.Compiler (compileAllAst)
import VirtualMachine.Vm (Compiled(..), serializeAndWriteCompiled, VmEnv,
    readAndDeserializeCompiled, execWithMain)
import Text.Printf (printf)
import Parser()
import Ast()
import InternalFunctions (astInternalEnv)
import VirtualMachine.InternalFunctions (vmInternalEnv)
import File (readFileEither)
import System.Environment()
import System.Exit ( exitWith, ExitCode (ExitFailure))
import System.Console.CmdArgs (whenLoud)
import System.FilePath ((</>), takeDirectory)
import System.Directory (canonicalizePath)
import Debug.Trace (traceM)
import Control.Monad (when)


-- | Check list
convertListToAST :: Env -> [GomExpr] -> EvalResult (Env, [GomAST])
convertListToAST env [] = pure (env, [])
convertListToAST env (ast:rest) = do
        (newEnv, result) <- gomExprToGomAST env ast
        (finalEnv, results) <- convertListToAST (newEnv ++ env) rest
        pure (finalEnv ++ newEnv, result : results)

codeToAST :: Env -> String -> Either ErrorMsg (Env, [GomAST])
codeToAST astEnv code =  do
    (gomexpr, _) <- case runParser parseCodeToGomExpr code of
        Right other -> Right other
        Left errList -> Left $ printErrors code errList
    result <- case convertListToAST astEnv gomexpr of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return result

codeToCompiled :: Env -> [GomExpr] -> Either ErrorMsg Compiled
codeToCompiled astEnv gomexpr = do
    (_, ast) <- case convertListToAST astEnv gomexpr of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    compiled <- case compileAllAst [] ast of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return compiled

resolveIncludePath :: FilePath -> FilePath -> FilePath
resolveIncludePath currentFilePath includePath =
    let currentDirectory = takeDirectory currentFilePath
    in currentDirectory </> includePath

getAllIncludes :: String -> [GomExpr] -> IO [GomExpr]
getAllIncludes srcPath gomexpr = getAllIncludesHelper srcPath gomexpr [srcPath]


getAllIncludesHelper :: String -> [GomExpr] -> [String] -> IO [GomExpr]
getAllIncludesHelper _ [] _ = pure []
getAllIncludesHelper srcPath (IncludeStatement _ path:rest) prevInc = do
    resolvedPath <- canonicalizePath $ resolveIncludePath srcPath path
    _ <- checkCircularDependency resolvedPath prevInc
    content <- readFileContent resolvedPath
    gomexpr <- parseContentToGomExpr content
    resRec <- getAllIncludesHelper resolvedPath gomexpr (prevInc
        ++ [resolvedPath])
    res <- getAllIncludesHelper srcPath rest (prevInc ++ [resolvedPath])
    return $ resRec ++ res
getAllIncludesHelper srcPath (current:rest) prevInc = do
    res <- getAllIncludesHelper srcPath rest prevInc
    return $ current : res

checkCircularDependency :: String -> [String] -> IO ()
checkCircularDependency resolvedPath prevInc =
    when (resolvedPath `elem` prevInc) $ putStrLn (
        "Circular dependency detected: " ++ resolvedPath)
        >> exitWith (ExitFailure 84
    )

readFileContent :: FilePath -> IO String
readFileContent path = do
    file <- readFileEither path
    case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> pure content

parseContentToGomExpr :: String -> IO [GomExpr]
parseContentToGomExpr content =
    case runParser parseCodeToGomExpr content of
        Left errList -> putStrLn (printErrors content errList)
            >> exitWith (ExitFailure 84)
        Right (gomexpr, _) -> pure gomexpr

codeToGomExprWithInclude :: String -> String -> IO [GomExpr]
codeToGomExprWithInclude srcPath code = do
    res <- case runParser parseCodeToGomExpr code of
        Right other -> return other
        Left errList -> putStrLn (printErrors code errList)
            >> exitWith (ExitFailure 84)
    case res of
        (gomexpr, _) -> getAllIncludes srcPath gomexpr

execBuild :: String -> String -> IO ()
execBuild src out = do
    file <- readFileEither src
    content <- case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> pure content
    gomexpr <- codeToGomExprWithInclude src content
    case codeToCompiled astInternalEnv gomexpr of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right compiled -> serializeAndWriteCompiled out compiled
            >> printf "Successfully compiled '%s' to '%s'.\n" src out

execRun :: String -> IO ()
execRun src = do
    readCompiled <- readAndDeserializeCompiled src
    comShow@(Compiled env compiled) <- case readCompiled of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right compiled -> pure compiled
    whenLoud $ putStrLn "Compiled instructions:" >> print comShow
    case execWithMain (env ++ vmInternalEnv) [] compiled [] of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right val -> print val
    return ()

-- | Parse GomExpr to annalise the syntaxe
runCode :: VmEnv -> String -> Either ErrorMsg Compiled
runCode env code = do
    (gomexpr, _) <- parseCodeToGomExprResult code
    (_, unevaluatedAst) <- case convertListToAST [] gomexpr of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    compiled <- case compileAllAst env unevaluatedAst of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return compiled

parseCodeToGomExprResult :: String -> Either ErrorMsg ([GomExpr], String)
parseCodeToGomExprResult code =
    case runParser parseCodeToGomExpr code of
        Right other -> Right other
        Left errList -> Left $ printErrors code errList
