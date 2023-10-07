{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Execution
-}

module Execution (runCode) where

import Parser (ErrorMsg, parseCodeToSExpr, Parser(..))
import Ast (Ast, evalAST, EvalResult (..), sexprToAST,
    EvalError(..), Env)

-- | Check list 
evalList :: Env -> [Ast] -> EvalResult (Env, [Ast])
evalList env [] = pure (env, [])
evalList env (ast:rest) = do
  (newEnv, result) <- evalSingle env ast
  (finalEnv, results) <- evalList newEnv rest
  pure (finalEnv, result : results)
    where
        evalSingle :: Env -> Ast -> EvalResult (Env, Ast)
        evalSingle env' ast' = evalAST env' ast'

-- | Execute all AST
runAllAst :: Env -> [Ast] -> Either ErrorMsg (Env, [Ast])
runAllAst env asts = case evalList env asts of
    EvalResult (Right results) -> Right results
    EvalResult (Left (EvalError msg _)) -> Left msg

-- | Parse SExpr to annalise the syntaxe
runCode :: Env -> String -> Either ErrorMsg (Env, [Ast])
runCode env code = do
    (sexpr, _) <- runParser parseCodeToSExpr code
    unevaluatedAst <- case traverse sexprToAST sexpr of
                Just ast -> Right ast
                Nothing -> Left "Could not parse SExpr"
    runAllAst env unevaluatedAst
