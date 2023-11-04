{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Execution
-}

module Execution (runCode, convertListToAST, execBuild, execRun) where

import Parser (ErrorMsg, parseCodeToGomExpr, Parser(..), ParseError(..),
    printErrors)
import Ast (GomAST (AGomIdentifier), EvalResult (..), gomExprToGomAST,
    EvalError(..), Env, GomExpr(..))
import VirtualMachine.Compiler (compileAllAst, getCompiledInsts, getCompiledEnv)
import VirtualMachine.Vm (Compiled(..), VmEnv(..), serializeAndWriteCompiled,
    readAndDeserializeCompiled, exec, execWithMain)
import Text.Printf (printf)
import Parser (parseCodeToGomExpr, runParser, printErrors)
import Ast (GomAST(..), Env, EvalResult(..), EvalError(..), gomExprToGomAST)
import InternalFunctions (internalEnv)
import File (readFileEither)
import System.Environment (getArgs)
import System.Exit ( exitWith, ExitCode (ExitFailure))
import System.Console.CmdArgs (whenLoud)

-- TODO: fix this file so that it handles new GomAST

-- runCode :: Env -> String -> Either ErrorMsg (Env, [GomAST])
-- runCode _ _ = Right ([], [AGomIdentifier "runCode is not implemented"])

-- convertEnvToVmEnv :: Env -> EvalResult (VmEnv)
-- convertEnvToVmEnv env@((key, value):rest) = 
--     (key, value) : convertEnvToVmEnv rest


-- | Check list
convertListToAST :: Env -> [GomExpr] -> EvalResult (Env, [GomAST])
convertListToAST env [] = pure (env, [])
convertListToAST env (ast:rest) = do
        (newEnv, result) <- gomExprToGomAST env ast
        (finalEnv, results) <- convertListToAST (newEnv ++ env) rest
        pure (finalEnv ++ newEnv, result : results)

-- -- | Execute all AST
-- runAllAst :: Env -> [GomAST] -> Either ErrorMsg (Env, [GomAST])
-- runAllAst env asts = case evalList env asts of
--     EvalResult (Right results) -> Right results
--     EvalResult (Left (EvalError msg _)) -> Left msg

codeToAST :: Env -> String -> Either ErrorMsg (Env, [GomAST])
codeToAST astEnv code =  do
    (gomexpr, _) <- case runParser parseCodeToGomExpr code of
        Right other -> Right other
        Left errList -> Left $ printErrors code errList
    result <- case convertListToAST astEnv gomexpr of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return result

codeToCompiled :: Env -> String -> Either ErrorMsg Compiled
codeToCompiled astEnv code = do
    (newAstEnv, ast) <- codeToAST astEnv code
    compiled <- case compileAllAst [] ast of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return compiled

fileExecution :: String -> IO ()
fileExecution path = do
    file <- readFileEither path
    content <- case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> pure content
    result <- case runCode [] content of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right result -> pure result
    putStrLn $ show result

execBuild :: String -> String -> IO ()
execBuild src out = do
    file <- readFileEither src
    content <- case file of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right content -> pure content
    compiled <- case codeToCompiled [] content of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right compiled -> pure compiled
    serializeAndWriteCompiled out compiled
    printf "Successfully compiled '%s' to '%s'.\n" src out

execRun :: String -> IO ()
execRun src = do
    readCompiled <- readAndDeserializeCompiled src
    (Compiled env compiled) <- case readCompiled of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right compiled -> pure compiled
    whenLoud $ print "Compiled instructions:" >> print env >> print compiled
    case execWithMain env [] compiled [] of
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
        Right val -> print val
    return ()

-- | Parse GomExpr to annalise the syntaxe
runCode :: VmEnv -> String -> Either ErrorMsg Compiled
runCode env code = do
    (gomexpr, _) <- case runParser parseCodeToGomExpr code of
        Right other -> Right other
        Left errList -> Left $ printErrors code errList
    (_, unevaluatedAst) <- case convertListToAST [] gomexpr of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    compiled <- case compileAllAst env unevaluatedAst of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return compiled

-- -- | Check list
-- evalList :: Env -> [GomAST] -> EvalResult (Env, [GomAST])
-- evalList env [] = pure (env, [])
-- evalList env (ast:rest) = do
--   (newEnv, result) <- evalSingle env ast
--   (finalEnv, results) <- evalList newEnv rest
--   pure (finalEnv, result : results)
--     where
--         evalSingle :: Env -> GomAST -> EvalResult (Env, GomAST)
--         evalSingle env' ast' = evalAST env' ast'

-- -- | Execute all AST
-- runAllAst :: Env -> [GomAST] -> Either ErrorMsg (Env, [GomAST])
-- runAllAst env asts = case evalList env asts of
--     EvalResult (Right results) -> Right results
--     EvalResult (Left (EvalError msg _)) -> Left msg
