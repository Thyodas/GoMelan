{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Compiler
-}

module VirtualMachine.Compiler () where

import Ast (Ast(..), EvalResult(..), EvalError(..), InternalFunction(..),
    sexprToAST, sexprToDefun, sexprToLambda)
import VirtualMachine.Vm (Instructions(..), Val(..), Operations(..), Stack,
    Insts, Compiled(..))

compileAst :: Ast -> EvalResult Compiled
compileAst (ADefine symbol expression) = do
    compiledExpression <- compileAst expression
    return $ Compiled (insts compiledExpression) (args compiledExpression)

compileAst' :: Ast -> EvalResult Compiled
compileAst' ast = case ast of
    ADefine { symbol = symbol, expression = expression } -> do
        compiledExpression <- compileAst expression
        return $ Compiled (insts compiledExpression) (args compiledExpression)
    ACall { function = function, arguments = arguments } -> do
        compiledArguments <- mapM compileAst arguments
        let compiledArgumentsInsts = concatMap insts compiledArguments
        let compiledArgumentsArgs = concatMap args compiledArguments
        return $ Compiled (compiledArgumentsInsts ++ [PushEnv function, Call])
            compiledArgumentsArgs
    ACondition { condition = condition, ifTrue = ifTrue, ifFalse = ifFalse } -> do
        compiledCondition <- compileAst condition
        compiledIfTrue <- compileAst ifTrue
        compiledIfFalse <- compileAst ifFalse
        return $ Compiled (insts compiledCondition ++ [JumpIfFalse (length (insts compiledIfTrue) + 1)] ++ insts compiledIfTrue ++ [JumpIfFalse (length (insts compiledIfFalse) + 1)] ++ insts compiledIfFalse) (args compiledCondition ++ args compiledIfTrue ++ args compiledIfFalse)
    ADefun { argumentNames = argumentNames, body = body } -> do
        compiledBody <- compileAst body
        return $ Compiled (insts compiledBody ++ [Ret]) (args compiledBody)
    AFunction { argumentNames = argumentNames, body = body } -> do
        compiledBody <- compileAst body
        return $ Compiled (insts compiledBody ++ [Ret]) (args compiledBody)
    AInternalFunction (InternalFunction function) -> do
        let compiledFunction = function []
        return $ Compiled (insts compiledFunction ++ [Ret]) (args compiledFunction)
    ANumber num -> return $ Compiled [Push (VNum num)] []
    ASymbol symbol -> return $ Compiled [PushEnv symbol] []
    AString str -> return $ Compiled [Push (VStr str)] []
    ABoolean bool -> return $ Compiled [Push (VBool bool)] []
