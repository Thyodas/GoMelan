{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Ast
-}

module Ast (
    GomExpr(..),
    GomExprType(..),
    GomAST(..),
    EvalError(..),
    EvalResult(..),
    InternalFunction(..),
    Env,
    EnumOperator(..),
    EnvKey,
    EnvValue,
    GomExprType(..),
    envInsert,
    throwEvalError,
    envLookup,
    extractSymbol,
    gomExprToGomAST,
    applyToSnd,
    envLookupEval,
    checkType,
    getAGomFunctionDefinition,
    typeResolver,
    gomExprToAGomFunctionCall,
    operatorToGomAST,
    getIdDetails,
    precedence,
    gomExprToAGomAssignment
) where

import Data.List (deleteBy, find, nub)
import VirtualMachine.Vm (EnumOperator(..))

data GomExprType = SingleType String | TypeList [GomExprType]
    deriving (Show, Eq)

data GomExpr = Number Int
    | Identifier String
    | GomString String
    | Boolean Bool
    | Type GomExprType
    | Statements [GomExpr]
    | Operator String
    | Term [GomExpr]
    | Expression [GomExpr]
    | List [GomExpr]
    | Block [GomExpr]
    | ParameterList [GomExpr]
    | ReturnStatement GomExpr
    | FunctionCall { functionName :: GomExpr, functionArguments :: GomExpr }
    | TypedIdentifier { identifier :: String, identifierType :: GomExpr}
    | IncludeStatement { includeList :: GomExpr, fromModule :: GomExpr }
    | Empty
    | Assignment { assignedIdentifier :: GomExpr, assignedExpression :: GomExpr }
    | ForLoopIter { forLoopInitialization :: GomExpr, forLoopCondition :: GomExpr,
                    forLoopUpdate :: GomExpr, forLoopIterBlock :: GomExpr }
    | Condition { gomIfCondition :: GomExpr, gomIfTrue :: GomExpr, gomIfFalse :: GomExpr }
    | Function { fnName :: String, fnArguments :: GomExpr, fnBody :: GomExpr, fnReturnType :: GomExpr }
    deriving (Show, Eq)

newtype InternalFunction = InternalFunction ([GomAST] -> EvalResult GomAST)

instance Show InternalFunction where
  show _ = "<Internal Function>"

instance Eq InternalFunction where
  _ == _ = True

data GomAST =
    AGomNumber Int
  | AGomIdentifier String
  | AGomStringLiteral String
  | AGomBooleanLiteral Bool
  | AGomType String
  | AGomTypeList [GomAST]
  | AGomStatements [GomAST]
  | AGomOperator EnumOperator
  | AGomTerm [GomAST]
  | AGomExpression [GomAST]
  | AGomList [GomAST]
  | AGomBlock [GomAST]
  | AGomFunctionArgument { aGomArgumentName :: GomAST, aGomArgumentType :: GomAST}
  | AGomParameterList [GomAST]
  | AGomInternalFunction InternalFunction
  | AGomReturnStatement GomAST
  | AGomFunctionCall { aGomFunctionName :: String, aGomFunctionArguments :: GomAST }
  | AGomTypedIdentifier { aGomIdentifier :: String, aGomIdentifierType :: GomAST }
  | AGomIncludeStatement { aGomIncludeList :: GomAST, aGomFromModule :: GomAST }
  | AGomEmpty
  | AGomAssignment { aGomAssignedIdentifier :: GomAST, aGomAssignedExpression :: GomAST }
  | AGomForLoop { aGomForLoopInitialization :: GomAST, aGomForLoopCondition :: GomAST, aGomForLoopUpdate :: GomAST, aGomForLoopIterBlock :: GomAST }
  | AGomCondition { aGomIfCondition :: GomAST, aGomIfTrue :: GomAST, aGomIfFalse :: GomAST }
  | AGomFunctionDefinition { aGomFnName :: String, aGomFnArguments :: GomAST, aGomFnBody :: GomAST, aGomFnReturnType :: GomAST }
  deriving (Show, Eq)

data EvalError = EvalError String [GomExpr]
  deriving (Eq, Show)

newtype EvalResult a = EvalResult { unEvalResult :: Either EvalError a }
  deriving (Show, Eq)

instance Functor EvalResult where
  fmap f (EvalResult (Right x)) = EvalResult (Right (f x))
  fmap _ (EvalResult (Left e)) = EvalResult (Left e)

instance Applicative EvalResult where
  pure = EvalResult . Right
  EvalResult (Left e) <*> _ = EvalResult (Left e)
  _ <*> EvalResult (Left e) = EvalResult (Left e)
  EvalResult (Right f) <*> EvalResult (Right x) = EvalResult (Right (f x))

instance MonadFail EvalResult where
  fail msg = EvalResult (Left (EvalError msg []))

instance Monad EvalResult where
  EvalResult (Left e) >>= _ = EvalResult (Left e)
  EvalResult (Right x) >>= f = f x
  -- EvalResult (Right (env, x)) >>= f = case f x of
  --   EvalResult (Left e) -> EvalResult (Left e)
  --   EvalResult (Right (_, x')) -> EvalResult (Right (env, x'))

applyToSnd :: (b -> c) -> (a, b) -> (a, c)
applyToSnd f (x, y) = (x, f y)

throwEvalError :: String -> [GomExpr] -> EvalResult a
throwEvalError msg expr = EvalResult (Left (EvalError msg expr))

gomExprListToGomASTList :: Env -> [GomExpr] -> EvalResult (Env, [GomAST])
gomExprListToGomASTList env list = do
  (_, allAst) <- traverse (gomExprToGomAST env) list >>= pure . unzip
  return ([], allAst)

gomExprListToGomASTListEnv :: Env -> [GomExpr] -> EvalResult (Env, [GomAST])
gomExprListToGomASTListEnv env [] = pure (env, [])
gomExprListToGomASTListEnv env (ast:rest) = do
  (newEnv, result) <- gomExprToGomAST env ast
  (finalEnv, results) <- gomExprListToGomASTListEnv (newEnv ++ env) rest
  pure (finalEnv ++ newEnv, result : results)

typeResolver :: Env -> GomAST -> EvalResult GomAST
typeResolver env (AGomFunctionCall s _) = do
  func <- envLookupEval env s
  case func of
    AGomFunctionDefinition { aGomFnReturnType=retType } -> pure retType
    _ -> throwEvalError ("Identifier '" ++ s ++ "' is not a function") []
typeResolver env (AGomIdentifier s) = do
  identifierValue <- envLookupEval env s
  typeResolver env identifierValue
typeResolver _ (AGomTypedIdentifier _ t) = pure t
typeResolver _ (AGomType t) = pure (AGomType t)
typeResolver _ (AGomTypeList t) = pure (AGomTypeList t)
typeResolver _ (AGomBooleanLiteral _) = pure (AGomType "Bool")
typeResolver _ (AGomNumber _) = pure (AGomType "Int")
typeResolver _ (AGomStringLiteral _) = pure (AGomType "String")
typeResolver _ (AGomOperator _) = pure (AGomType "Operator")
typeResolver env (AGomList elements) = do
  types <- traverse (typeResolver env) elements
  let uniqueTypes = nub types
  case uniqueTypes of
    [] -> throwEvalError "Empty List" []
    [singleType] -> pure $ AGomTypeList uniqueTypes
    tList -> throwEvalError ("Types mismatch in list, found '" ++ show tList ++ "'") []
typeResolver env (AGomExpression exprs) = do
  types <- traverse (typeResolver env) exprs
  let uniqueTypes = nub (filter (/= AGomType "Operator") types)
  case uniqueTypes of
    [] -> throwEvalError "Empty expression" []
    [singleType] -> pure singleType
    tList -> throwEvalError ("Types mismatch in expression, found '" ++ show tList ++ "'") []
typeResolver _ ast = throwEvalError ("Couldn't resolve type for '"
  ++ show ast ++ "'.") []

-- | Check if type is valid recursively
-- | Takes the GomAST to check and a type to check its resolution with
checkType :: Env -> GomAST -> GomAST -> EvalResult GomAST
checkType env astA astB = do
  resolvedA <- typeResolver env astA
  resolvedB <- typeResolver env astB
  if resolvedA == resolvedB
    then pure resolvedA
    else throwEvalError
      ("Type mismatch, found '" ++ show resolvedA ++ "' but expected '"
      ++ show resolvedB ++ "'.") []

-- TODO: finish this function (missing transformation of block and so on)
getAGomFunctionDefinition :: Env -> String -> EvalResult GomAST
getAGomFunctionDefinition env name = do
  func <- envLookupEval env name
  case func of
    f@AGomFunctionDefinition {aGomFnArguments=(AGomParameterList _)} ->
        pure f
    AGomFunctionDefinition {} ->
      throwEvalError ("Function '" ++ name ++ "' has invalid arguments") []
    _ -> throwEvalError ("Identifier '" ++ name
      ++ "' is not a function") []

gomExprToAGomFunctionCall :: Env -> GomExpr -> EvalResult (Env, GomAST)
gomExprToAGomFunctionCall env (FunctionCall (Identifier name)
  (ParameterList args)) = do
  (_, argsAst) <- gomExprListToGomASTList env args
  AGomFunctionDefinition {aGomFnArguments=(AGomParameterList funcDefArgs)} <-
    getAGomFunctionDefinition env name
  let funcDegArgsTypes = map aGomArgumentType funcDefArgs
  _ <- traverse (uncurry (checkType env)) (zip argsAst funcDegArgsTypes)
  return $ (env, AGomFunctionCall name (AGomList argsAst))

-- | Error handling
gomExprToAGomFunctionCall _ (FunctionCall (Identifier _) param) =
    throwEvalError "Expected a ParameterList" [param]
gomExprToAGomFunctionCall _ (FunctionCall name _) = throwEvalError
    "Expected an Identifier" [name]
gomExprToAGomFunctionCall _ _ = throwEvalError "Expected a FunctionCall" []

getIdDetails :: Env -> GomAST -> EvalResult (String, GomAST)
getIdDetails env (AGomIdentifier name) = do
  value <- envLookupEval env name
  return (name, value)
getIdDetails env (AGomTypedIdentifier name t) = do
  _ <- case envLookup env name of
    Just _ -> throwEvalError ("Cannot redeclare '" ++ name
      ++ "' already exists") []
    Nothing -> pure ()
  return (name, t)
getIdDetails _ _ = throwEvalError "Expected an Identifier" []

gomExprToAGomAssignment :: Env -> GomExpr -> EvalResult (Env, GomAST)
gomExprToAGomAssignment env (Assignment idExpr valExpr) = do
  (_, idGomAST) <- gomExprToGomAST env idExpr
  (_, valGomAST) <- gomExprToGomAST env valExpr
  (idName, typeToCheck) <- getIdDetails env idGomAST
  _ <- checkType env valGomAST typeToCheck
  return ([(idName, valGomAST)], AGomAssignment idGomAST valGomAST)
gomExprToAGomAssignment _ got = throwEvalError "Expected an Assignment" [got]

precedence :: GomExpr -> Int
precedence (Operator op) = case op of
  "+" -> 1
  "-" -> 1
  "*" -> 2
  "/" -> 2
  "%" -> 2
  "==" -> 3
  "!=" -> 3
  "<=" -> 3
  ">=" -> 3
  "<" -> 3
  ">" -> 3
  "&&" -> 4
  "!" -> 5
  _ -> 0
precedence _ = 0

shuntingYard :: [GomExpr] -> [GomExpr]
shuntingYard expr = reverse $ shuntingYard' expr [] []

shuntingYard' :: [GomExpr] -> [GomExpr] -> [GomExpr] -> [GomExpr]
shuntingYard' [] outputStack operatorStack = outputStack ++ reverse operatorStack
shuntingYard' (e:expr) outputStack operatorStack =
  case e of
    op@(Operator _) ->
      let (oStack, oQueue) = span (\x -> precedence op <= precedence x) operatorStack
      in shuntingYard' expr (outputStack ++ oQueue) (op:oStack)
    (Number _) -> shuntingYard' expr (outputStack ++ [e]) operatorStack
    other -> shuntingYard' expr (outputStack ++ [other]) operatorStack

gomExprListToGomASTListShuntingYard :: Env -> [GomExpr] -> EvalResult (Env, [GomAST])
gomExprListToGomASTListShuntingYard env exprList = do
  let postFixExpr = shuntingYard exprList
  (_, allAst) <- traverse (gomExprToGomAST env) postFixExpr >>= pure . unzip
  return ([], reverse allAst)

removeNewAssignment :: Env -> Env -> Env
removeNewAssignment globalEnv env = filter (not . (isNew globalEnv)) env
    where
      isNew :: Env -> EnvEntry -> Bool
      isNew _ (_, AGomAssignment _ _) = True
      isNew globalEnv' (key, _) = case envLookup globalEnv' key of
        Just _ -> False
        Nothing -> True

gomExprToGomAST :: Env -> GomExpr -> EvalResult ([EnvEntry], GomAST)
gomExprToGomAST _ (Number n) = pure ([], AGomNumber n)
gomExprToGomAST _ (Identifier s) = pure ([], AGomIdentifier s)
gomExprToGomAST _ (GomString s) = pure ([], AGomStringLiteral s)
gomExprToGomAST _ (Boolean b) = pure ([], AGomBooleanLiteral b)
gomExprToGomAST _ (Type (SingleType t)) = pure ([], AGomType t)
gomExprToGomAST env (Type (TypeList t)) = do
  (_, t') <- traverse (gomExprToGomAST env . Type) t >>= pure . unzip
  return ([], AGomTypeList t')
gomExprToGomAST env (Statements s) = do
  (_, allAst) <- gomExprListToGomASTList env s
  return ([], AGomStatements allAst)

gomExprToGomAST _ op@(Operator _) = do
  result <- operatorToGomAST op
  return ([], result)
gomExprToGomAST env (Term t) = applyToSnd AGomTerm <$>
    gomExprListToGomASTList env t
gomExprToGomAST env (Expression e) = applyToSnd AGomExpression <$>
    gomExprListToGomASTListShuntingYard env e
gomExprToGomAST env (List l) = applyToSnd AGomList <$>
    gomExprListToGomASTList env l
gomExprToGomAST env (Block b) = applyToSnd AGomBlock <$>
    gomExprListToGomASTListEnv env b
gomExprToGomAST env (ParameterList p) = applyToSnd AGomParameterList <$>
    gomExprListToGomASTList env p
gomExprToGomAST env function@(FunctionCall _ _) = gomExprToAGomFunctionCall
    env function
gomExprToGomAST env (TypedIdentifier name t) = do
  (_, t') <- gomExprToGomAST env t
  return ([], AGomTypedIdentifier name t')
gomExprToGomAST env (IncludeStatement i m) = do
  (_, i') <- gomExprToGomAST env i
  (_, m') <- gomExprToGomAST env m
  return ([], AGomIncludeStatement i' m')
gomExprToGomAST env a@(Assignment _ _) = gomExprToAGomAssignment env a
gomExprToGomAST _ Empty = pure ([], AGomEmpty)
gomExprToGomAST env (ForLoopIter init cond update block) = do
  (_, init') <- gomExprToGomAST env init
  (_, cond') <- gomExprToGomAST env cond
  (_, update') <- gomExprToGomAST env update
  (blockEnv, block') <- gomExprToGomAST env block
  return (removeNewAssignment env blockEnv, AGomForLoop init' cond' update' block')
gomExprToGomAST env (Condition cond true false) = do
  (_, cond') <- gomExprToGomAST env cond
  (trueEnv, true') <- gomExprToGomAST env true
  (falseEnv, false') <- gomExprToGomAST env false
  return (removeNewAssignment env trueEnv ++ removeNewAssignment env falseEnv,
    AGomCondition cond' true' false')
gomExprToGomAST env (Function name args body retType) = do
  (_, args') <- gomExprToGomAST env args
  (_, retType') <- gomExprToGomAST env retType
  let tempFunction = AGomFunctionDefinition name args' (AGomBlock []) retType'
  (newEnv, body') <- gomExprToGomAST ((name, tempFunction) : env) body
  return (removeNewAssignment env newEnv,
    AGomFunctionDefinition name args' body' retType')
gomExprToGomAST env (ReturnStatement expr) = do
  (_, expr') <- gomExprToGomAST env expr
  return ([], AGomReturnStatement expr')

operatorToGomAST :: GomExpr -> EvalResult GomAST
operatorToGomAST (Operator "+") = pure (AGomOperator SignPlus)
operatorToGomAST (Operator "-") = pure (AGomOperator SignMinus)
operatorToGomAST (Operator "*") = pure (AGomOperator SignMultiply)
operatorToGomAST (Operator "/") = pure (AGomOperator SignDivide)
operatorToGomAST (Operator "%") = pure (AGomOperator SignModulo)
operatorToGomAST (Operator "==") = pure (AGomOperator SignEqual)
operatorToGomAST (Operator "!=") = pure (AGomOperator SignNotEqual)
operatorToGomAST (Operator "!") = pure (AGomOperator SignNot)
operatorToGomAST (Operator "&&") = pure (AGomOperator SignAnd)
operatorToGomAST (Operator "<=") = pure (AGomOperator SignInfEqual)
operatorToGomAST (Operator ">=") = pure (AGomOperator SignSupEqual)
operatorToGomAST (Operator "<") = pure (AGomOperator SignInf)
operatorToGomAST (Operator ">") = pure (AGomOperator SignSup)
operatorToGomAST (Operator op) = throwEvalError ("Unknown operator '" ++ op ++ "'") []
operatorToGomAST _ = throwEvalError "Expected an Operator" []

extractSymbol :: GomExpr -> Maybe String
extractSymbol (Identifier s) = Just s
extractSymbol _ = Nothing

type Env = [EnvEntry]
type EnvEntry = (EnvKey, EnvValue)
type EnvKey = String
type EnvValue = GomAST

-- | Insert element in env
envInsert :: Env -> EnvKey -> EnvValue -> Env
envInsert env key value = newEntry : deleteBy checkKey newEntry env
  where
    checkKey :: EnvEntry -> EnvEntry -> Bool
    checkKey (sym, _) (sym2, _) = sym == sym2

    newEntry :: EnvEntry
    newEntry = (key, value)

-- | Check if element is in env
envLookup :: Env -> EnvKey -> Maybe EnvValue
envLookup env key = find checkKey env >>= Just . snd
  where
    checkKey :: EnvEntry -> Bool
    checkKey (sym, _) = sym == key

-- | Check if element is in env with EvalResult
envLookupEval :: Env -> EnvKey -> EvalResult EnvValue
envLookupEval env key = case envLookup env key of
  Just val -> pure val
  Nothing -> throwEvalError ("Identifier '" ++ key ++ "' not found in env") []
