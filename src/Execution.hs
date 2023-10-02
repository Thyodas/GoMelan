module Execution where

import Parser (ErrorMsg, parseCodeToSExpr, Parser(..))
import Ast (Ast, evalAST, EvalResult (..), sexprToAST,
    EvalError(..), Env)

evalList :: Env -> [Ast] -> EvalResult (Env, [Ast])
evalList env [] = pure (env, [])
evalList env (ast:rest) = do
  (newEnv, result) <- evalSingle env ast
  (finalEnv, results) <- evalList newEnv rest
  pure (finalEnv, result : results)
    where
        evalSingle :: Env -> Ast -> EvalResult (Env, Ast)
        evalSingle env ast = evalAST env ast

runAllAst :: Env -> [Ast] -> Either ErrorMsg [Ast]
runAllAst env asts = case evalList env asts of
    EvalResult (Right (_, results)) -> Right results
    EvalResult (Left (EvalError msg _)) -> Left msg

runCode :: Env -> String -> Either ErrorMsg [Ast]
runCode env code = do
    (sexpr, a) <- runParser parseCodeToSExpr code
    unevaluatedAst <- case traverse sexprToAST sexpr of
                Just ast -> Right ast
                Nothing -> Left "Could not parse SExpr"
    runAllAst env unevaluatedAst
