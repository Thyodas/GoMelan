{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Ast
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Ast (
    GomExpr(..),
    GomExprType(..),
    GomAST(..),
    EvalError(..),
    EvalResult(..),
    InternalFunction(..),
    Env,
    EnvKey,
    EnvValue,
    envInsert,
    throwEvalError,
    envLookup,
    extractSymbol,
    gomExprToGomAST
) where

import Data.List (deleteBy, find)

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
    | FunctionCall { functionName :: GomExpr, functionArguments :: GomExpr }
    | TypedIdentifier { identifier :: GomExpr, identifierType :: GomExpr}
    | IncludeStatement { includeList :: GomExpr, fromModule :: GomExpr }
    | Empty
    | Assignment { assignedIdentifier :: GomExpr, assignedExpression :: GomExpr }
    | ForLoopIter { forLoopInitialization :: GomExpr, forLoopCondition :: GomExpr,
                    forLoopUpdate :: GomExpr, forLoopIterBlock :: GomExpr }
    | Condition { gomIfCondition :: GomExpr, gomIfTrue :: GomExpr, gomIfFalse :: GomExpr }
    | Function { fnName :: GomExpr, fnArguments :: GomExpr, fnBody :: GomExpr, fnReturnType :: GomExpr }
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
  | AGomOperator String
  | AGomTerm [GomAST]
  | AGomExpression [GomAST]
  | AGomList [GomAST]
  | AGomBlock [GomAST]
  | AGomFunctionArgument { aGomArgumentName :: GomAST, aGomArgumentType :: GomAST}
  | AGomParameterList [GomAST]
  | AGomInternalFunction InternalFunction
  | AGomFunctionCall { aGomFunctionName :: GomAST, aGomFunctionArguments :: GomAST }
  | AGomTypedIdentifier { aGomIdentifier :: GomAST, aGomIdentifierType :: GomAST }
  | AGomIncludeStatement { aGomIncludeList :: GomAST, aGomFromModule :: GomAST }
  | AGomEmpty
  | AGomAssignment { aGomAssignedIdentifier :: GomAST, aGomAssignedExpression :: GomAST }
  | AGomForLoop { aGomForLoopInitialization :: GomAST, aGomForLoopCondition :: GomAST, aGomForLoopUpdate :: GomAST, aGomForLoopIterBlock :: GomAST }
  | AGomCondition { aGomIfCondition :: GomAST, aGomIfTrue :: GomAST, aGomIfFalse :: GomAST }
  | AGomFunctionDefinition { aGomFnName :: GomAST, aGomFnArguments :: GomAST, aGomFnBody :: GomAST, aGomFnReturnType :: GomAST }
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

checkCallArg :: GomAST -> EvalResult (Env, GomAST)
checkCallArg ast@(AGomIdentifier s) = pure ([], ast)
checkCallArg ast@(AGomFunctionCall _ _) = pure ([], ast)
checkCallArg ast@(AGomExpression _) = pure ([], ast)
checkCallArg ast@(AGomTerm _) = pure ([], ast)
checkCallArg ast@(AGomList _) = pure ([], ast)
checkCallArg ast@(AGomBooleanLiteral _) = pure ([], ast)
checkCallArg ast@(AGomNumber _) = pure ([], ast)
checkCallArg ast@(AGomStringLiteral _) = pure ([], ast)
checkCallArg ast = throwEvalError "Invalid argument type in function call" []

-- | Check if type is valid recursively
-- | Takes the GomAST to check and a type to check its resolution with
checkType :: Env -> GomAST -> GomAST -> Maybe GomAST
-- GomAST to type resolution
checkType env (AGomFunctionCall (AGomIdentifier s) _) t = do
  AGomFunctionDefinition { aGomFnReturnType=retType } <- envLookup env s
  checkType env retType t
checkType env (AGomIdentifier s) t = checkType env (AGomType s) t

-- Type checking
checkType _ (AGomNumber _) (AGomType "Int") = Just (AGomType "Int")
checkType _ (AGomStringLiteral _) (AGomType "String") = Just (AGomType "String")
checkType _ (AGomBooleanLiteral _) (AGomType "Bool") = Just (AGomType "Bool")
checkType _ (AGomTypeList a) (AGomTypeList b) = do
  checked <- traverse (uncurry (checkType undefined)) (zip a b)
  Just (AGomTypeList checked)
checkType _ (AGomType s) (AGomType t)
  | s == t = Just (AGomType s)
  | otherwise = Nothing

-- Error handling
checkType _ _ _ = Nothing

gomExprToAGomFunctionCall :: Env -> GomExpr -> EvalResult (Env, GomAST)
gomExprToAGomFunctionCall env (FunctionCall nameId@(Identifier name) (ParameterList args)) = do
  (_, argsAst) <- gomExprListToGomASTList env args
  (AGomFunctionDefinition {aGomFnArguments=(AGomParameterList funcDefArgs)}) <-
    case envLookup env name of
      Just f@(AGomFunctionDefinition {}) -> pure f
      Just _ -> throwEvalError ("Identifier '" ++ name
        ++ "' is not a function") []
      Nothing -> throwEvalError ("Function '" ++ name ++ "' not found") [nameId]
  let funcDegArgsTypes = map aGomArgumentType funcDefArgs
  _ <- case traverse (uncurry (checkType env)) (zip argsAst funcDegArgsTypes) of
    Just _ -> pure ()
    Nothing -> throwEvalError ("Invalid argument type in function call") []
  return $ (env, AGomFunctionCall (AGomIdentifier name) (AGomList argsAst))
gomExprToAGomFunctionCall _ (FunctionCall (Identifier _) param) = throwEvalError "Expected a ParameterList" [param]
gomExprToAGomFunctionCall _ (FunctionCall name _) = throwEvalError "Expected an Identifier" [name]


gomExprToGomAST :: Env -> GomExpr -> EvalResult (Env, GomAST)
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

gomExprToGomAST _ (Operator s) = pure ([], AGomOperator s)
gomExprToGomAST env (Term t) = applyToSnd AGomTerm <$> gomExprListToGomASTList env t
gomExprToGomAST env (Expression e) = applyToSnd AGomExpression <$> gomExprListToGomASTList env e
gomExprToGomAST env (List l) = applyToSnd AGomList <$> gomExprListToGomASTList env l
gomExprToGomAST env (Block b) = applyToSnd AGomBlock <$> gomExprListToGomASTList env b
gomExprToGomAST env (ParameterList p) = applyToSnd AGomParameterList <$> gomExprListToGomASTList env p
gomExprToGomAST env function@(FunctionCall _ _) = gomExprToAGomFunctionCall env function

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
