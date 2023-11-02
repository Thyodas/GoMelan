{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Compiler
-}

module VirtualMachine.Compiler () where



import Ast (GomAST(..), EvalResult(..), EvalError(..), InternalFunction(..),
    throwEvalError)
import VirtualMachine.Vm (Instructions(..), Val(..), Stack,
    Insts, Compiled(..), EnumOperator(..), VmEnv(..))

getCompiledEnv :: Compiled -> VmEnv
getCompiledEnv (Compiled env _) = env

getCompiledInsts :: Compiled -> Insts
getCompiledInsts (Compiled _ insts) = insts

compileAst :: VmEnv -> GomAST -> EvalResult Compiled
compileAst _ (AGomNumber x) = pure $ Compiled [] [Push (VNum x)]
compileAst _ (AGomBooleanLiteral x) = pure $ Compiled [] [Push (VBool x)]
compileAst _ (AGomStringLiteral x) = pure $ Compiled [] [Push (VStr x)]

compileAst _ (AGomEmpty) = pure $ Compiled [] []

compileAst env (AGomParameterList params) = do
    compiledParams <- mapM (compileAst env) params
    return $ Compiled [] (concatMap getCompiledInsts compiledParams)

compileAst env (AGomFunctionCall name args) = do
    compiledArgs <- compileAst env args
    let compiledArgsInsts = getCompiledInsts compiledArgs
    return $ Compiled [] (compiledArgsInsts ++ [PushEnv name, Call])
compileAst _ (AGomOperator op) = pure $ Compiled [] [Push (VOp op), Call]

compileAst env (AGomExpression exprList) = do
    compiledExprs <- mapM (compileAst env) exprList
    let declarations = concatMap getCompiledInsts compiledExprs
    return $ Compiled [] declarations

compileAst env (AGomCondition cond thenExpr elseExpr) = do
    compiledCond <- compileAst env cond
    compiledThen <- compileAst env thenExpr
    compiledElse <- compileAst env elseExpr
    let compiledCondInsts = getCompiledInsts compiledCond
    let compiledThenInsts = getCompiledInsts compiledThen
    let compiledThenEnv = getCompiledEnv compiledThen
    let compiledElseInsts = getCompiledInsts compiledElse
    let compiledElseEnv = getCompiledEnv compiledElse
    let compiledInsts = compiledCondInsts
            ++ [JumpIfFalse (length compiledThenInsts + 1)]
            ++ compiledThenInsts ++ [Jump (length compiledElseInsts)]
            ++ compiledElseInsts
    return $ Compiled (compiledThenEnv ++ compiledElseEnv) compiledInsts


compileAst _ unknown = throwEvalError ("Not implemented yet: '"
    ++ show unknown ++ "'.") []

compileAllAst :: VmEnv -> [GomAST] -> EvalResult Compiled
compileAllAst env asts = do
    compiledAsts <- mapM (compileAst env) asts
    let compiledEnv = concatMap getCompiledEnv compiledAsts
    let compiledInsts = concatMap getCompiledInsts compiledAsts
    return $ Compiled compiledEnv compiledInsts
