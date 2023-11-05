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
    compiledInit <- compileAst env lInit
    compiledCond <- compileAst env lCond
    compiledUpdate <- compileAst env lUpdate
    compiledBody <- compileAst env lBody
    let compiledInitInsts = getCompiledInsts compiledInit
    let compiledCondInsts = getCompiledInsts compiledCond
    let compiledUpdateInsts = getCompiledInsts compiledUpdate
    let compiledBodyInsts = getCompiledInsts compiledBody
    let compiledBodyEnv = getCompiledEnv compiledBody
    return $ Compiled compiledBodyEnv (compiledInitInsts ++ compiledCondInsts
            ++ [JumpIfFalse (length compiledBodyInsts + length
                            compiledUpdateInsts + 1)]
            ++ compiledBodyInsts ++ compiledUpdateInsts
            ++ [Jump (-(length compiledBodyInsts
            + length compiledUpdateInsts + length compiledCondInsts + 1))])

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
    return $ Compiled [] (compiledListInsts ++ compiledIndexInsts ++ [AccessList])

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
