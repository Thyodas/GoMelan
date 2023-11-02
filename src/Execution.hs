{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Execution
-}

module Execution (runCode, convertListToAST) where

import Parser (ErrorMsg, parseCodeToGomExpr, Parser(..), ParseError(..),
    printErrors)
import Ast (GomAST (AGomIdentifier), EvalResult (..), gomExprToGomAST,
    EvalError(..), Env, GomExpr(..))
import VirtualMachine.Compiler (compileAllAst, getCompiledInsts, getCompiledEnv)
import VirtualMachine.Vm (Compiled(..), VmEnv(..))

-- TODO: fix this file so that it handles new GomAST

-- runCode :: Env -> String -> Either ErrorMsg (Env, [GomAST])
-- runCode _ _ = Right ([], [AGomIdentifier "runCode is not implemented"])


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
