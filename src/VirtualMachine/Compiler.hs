{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Compiler
-}

module VirtualMachine.Compiler (compileAllAst, getCompiledInsts,
    getCompiledEnv) where

import Ast (GomAST(..), EvalResult(..), EvalError(..),
    throwEvalError)
import VirtualMachine.Vm (Instructions(..), Val(..), Stack,
    Insts, Compiled(..), EnumOperator(..), VmEnv(..), getOperationNbArgs)

getCompiledEnv :: Compiled -> VmEnv
getCompiledEnv (Compiled env _) = env

getCompiledInsts :: Compiled -> Insts
getCompiledInsts (Compiled _ insts) = insts

compileAst :: VmEnv -> GomAST -> EvalResult Compiled
compileAst _ (AGomNumber x) = pure $ Compiled [] [Push (VNum x)]
compileAst _ (AGomBooleanLiteral x) = pure $ Compiled [] [Push (VBool x)]
compileAst _ (AGomStringLiteral x) = pure $ Compiled [] [Push (VStr x)]

compileAst _ (AGomEmpty) = pure $ Compiled [] []

compileAst _ (AGomIdentifier name) = pure $ Compiled [] [PushEnv name]

compileAst env (AGomParameterList params) = do
    compiledParams <- mapM (compileAst env) params
    return $ Compiled [] (concatMap getCompiledInsts compiledParams)

compileAst env (AGomFunctionCall name argList@(AGomParameterList args)) = do
    compiledArgs <- compileAst env argList
    let compiledArgsInsts = getCompiledInsts compiledArgs
    return $ Compiled [] (compiledArgsInsts
        ++ [PushEnv name, Call (length args)])
compileAst _ (AGomOperator op) = pure $ Compiled [] [Push (VOp op),
    Call (getOperationNbArgs op)]

compileAst env (AGomExpression exprList) = do
    compiledExprs <- mapM (compileAst env) exprList
    let declarations = concatMap getCompiledInsts compiledExprs
    return $ Compiled [] declarations

compileAst env (AGomCondition cond thenExpr elseExpr) = do
    cCond <- compileAst env cond; cThen <- compileAst env thenExpr
    cElse <- compileAst env elseExpr
    let cCompI = getCompiledInsts cCond; cThenI = getCompiledInsts cThen
        cThenE = getCompiledEnv cThen; cElseI = getCompiledInsts cElse
        cElseE = getCompiledEnv cElse; ciledInsts = cCompI ++ 
            [JumpIfFalse (length cThenI + 1)] ++ cThenI ++
            [Jump (length cElseI)] ++ cElseI
    return $ Compiled (cThenE ++ cElseE) ciledInsts

compileAst env (AGomAssignment idName expr) = do
    name <- case idName of
        AGomTypedIdentifier name _ -> pure name
        AGomIdentifier name -> pure name
        _ -> throwEvalError "Invalid assignment identifier." []
    compiledExpr <- compileAst env expr
    let compiledExprInsts = getCompiledInsts compiledExpr
    return $ Compiled [] (compiledExprInsts ++ [AddEnv name])

compileAst env (AGomFunctionDefinition fnName (AGomParameterList fnArgs) fnBody
    _) = do
    compiledBody <- compileAst env fnBody
    let args = concat [[PushArg i, AddEnv el] | (i, AGomTypedIdentifier el _)
                   <- zip [0..] fnArgs]
    let compiledBodyInsts = args ++ getCompiledInsts compiledBody
    return $ Compiled [(fnName, VFunction compiledBodyInsts)] []

compileAst env (AGomForLoop lInit lCond lUpdate lBody) = do
    cInit <- compileAst env lInit; compiledCond <- compileAst env lCond
    cUp <- compileAst env lUpdate; compiledBody <- compileAst env lBody
    let cInitI = getCompiledInsts cInit; cCondI = getCompiledInsts compiledCond
    let cUpdateI = getCompiledInsts cUp; cBodyI = getCompiledInsts compiledBody
    let cBodyE = getCompiledEnv compiledBody
    return $ Compiled cBodyE (cInitI ++ cCondI ++ [JumpIfFalse (length cBodyI +
        length cUpdateI + 1)] ++ cBodyI ++ cUpdateI ++ [Jump (-(length cBodyI
        + length cUpdateI + length cCondI + 1))])

compileAst env (AGomBlock block) = do
    compiledBlock <- compileAllAst env block
    let compiledBlockInsts = getCompiledInsts compiledBlock
    let compiledBlockEnv = getCompiledEnv compiledBlock
    return $ Compiled compiledBlockEnv compiledBlockInsts

compileAst env (AGomReturnStatement expr) = do
    compiledExpr <- compileAst env expr
    let compiledExprInsts = getCompiledInsts compiledExpr
    return $ Compiled [] (compiledExprInsts ++ [Ret])

compileAst env (AGomList list) = do
    compiledList <- compileAllAst env list
    let compiledListInsts = getCompiledInsts compiledList
    -- let compiledListEnv = getCompiledEnv compiledList
    return $ Compiled [] (compiledListInsts ++ [BuildList (length list)])

compileAst env (AGomAccess list index) = do
    compiledList <- compileAst env list
    compiledIndex <- compileAst env index
    let compiledListInsts = getCompiledInsts compiledList
    let compiledIndexInsts = getCompiledInsts compiledIndex
    return $ Compiled [] (compiledListInsts ++ compiledIndexInsts ++
        [AccessList])

compileAst _ (AGomFunctionPrototype _ _ _) = pure $ Compiled [] []

compileAst _ unknown = throwEvalError ("Not implemented yet: '"
    ++ show unknown ++ "'.") []

compileAllAst :: VmEnv -> [GomAST] -> EvalResult Compiled
compileAllAst _ [] = pure $ Compiled [] []
compileAllAst env (ast:rest) = do
    compiledAst <- compileAst env ast
    let compiledAstEnv = getCompiledEnv compiledAst
    compiledRest <- compileAllAst (compiledAstEnv ++ env) rest
    let compiledAstInsts = getCompiledInsts compiledAst
    let compiledRestInsts = getCompiledInsts compiledRest
    let compiledRestEnv = getCompiledEnv compiledRest
    return $ Compiled (compiledAstEnv ++ compiledRestEnv)
        (compiledAstInsts ++ compiledRestInsts)
