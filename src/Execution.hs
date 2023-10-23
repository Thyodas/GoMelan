{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Execution
-}

module Execution (runCode) where

import Parser (ErrorMsg, parseCodeToGomExpr, Parser(..))
import Ast (Ast, evalAST, EvalResult (..), gomexprToAST,
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

-- | Parse GomExpr to annalise the syntaxe
runCode :: Env -> String -> Either ErrorMsg (Env, [Ast])
runCode env code = do
    (gomexpr, _) <- runParser parseCodeToGomExpr code
    unevaluatedAst <- case traverse gomexprToAST gomexpr of
                Just ast -> Right ast
                Nothing -> Left "Could not parse GomExpr"
    runAllAst env unevaluatedAst
