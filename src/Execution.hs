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

-- TODO: fix this file so that it handles new GomAST

-- runCode :: Env -> String -> Either ErrorMsg (Env, [GomAST])
-- runCode _ _ = Right ([], [AGomIdentifier "runCode is not implemented"])


-- | Check list
convertListToAST :: Env -> [GomExpr] -> EvalResult (Env, [GomAST])
convertListToAST env [] = pure (env, [])
convertListToAST env (ast:rest) = do
  (newEnv, result) <- gomExprToGomAST env ast
  (finalEnv, results) <- convertListToAST newEnv rest
  pure (finalEnv, result : results)

-- -- | Execute all AST
-- runAllAst :: Env -> [GomAST] -> Either ErrorMsg (Env, [GomAST])
-- runAllAst env asts = case evalList env asts of
--     EvalResult (Right results) -> Right results
--     EvalResult (Left (EvalError msg _)) -> Left msg

-- | Parse GomExpr to annalise the syntaxe
runCode :: Env -> String -> Either ErrorMsg (Env, [GomAST])
runCode env code = do
    (gomexpr, _) <- case runParser parseCodeToGomExpr code of
        Right other -> Right other
        Left errList -> Left $ printErrors code errList
    (newEnv, unevaluatedAst) <- case convertListToAST env gomexpr of
        EvalResult (Right results) -> Right results
        EvalResult (Left (EvalError msg _)) -> Left msg
    return (newEnv, unevaluatedAst)
