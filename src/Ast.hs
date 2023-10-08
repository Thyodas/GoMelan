{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Ast
-}

module Ast (
    sexprToAST,
    SExpr(..),
    Ast(..),
    EvalError(..),
    EvalResult(..),
    InternalFunction(..),
    evalAST,
    evalASTCall,
    Env,
    EnvKey,
    EnvValue,
    envInsert,
    throwEvalError,
    envLookup,
    sexprToDefun,
    sexprToLambda,
    extractSymbol,
    evalASTCondition
) where

import Data.List (deleteBy, find)

data SExpr = Number Int
    | Symbol String
    | Boolean Bool
    | List [SExpr]
    deriving (Show, Eq)

newtype InternalFunction = InternalFunction ([Ast] -> EvalResult Ast)

instance Show InternalFunction where
  show _ = "<Internal Function>"

instance Eq InternalFunction where
  _ == _ = True

data Ast = ADefine { symbol :: String, expression :: Ast }
        | ACall { function :: String, arguments :: [Ast] }
        | ACondition { condition :: Ast, ifTrue :: Ast, ifFalse :: Ast }
        | ADefun { argumentNames :: [String], body :: Ast }
        | AFunction { argumentNames :: [String], body :: Ast }
        | AInternalFunction InternalFunction
        | ANumber Int
        | ASymbol String
        | AString String
        | ABoolean Bool
    deriving (Show, Eq)

data EvalError = EvalError String [Ast]
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

instance Monad EvalResult where
  EvalResult (Left e) >>= _ = EvalResult (Left e)
  EvalResult (Right x) >>= f = f x

throwEvalError :: String -> [Ast] -> EvalResult a
throwEvalError msg asts = EvalResult (Left (EvalError msg asts))

-- | Convert SExpr to AST
sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Number n) = Just (ANumber n)
sexprToAST (Symbol s) = Just (ASymbol s)
sexprToAST (Boolean b) = Just (ABoolean b)
sexprToAST (List [Symbol "define", Symbol s, e]) = sexprToDefine s e
sexprToAST (List [Symbol "defun", name, params, core]) =
    sexprToDefun name params core
sexprToAST (List [Symbol "define", List (Symbol name:params), core]) =
    sexprToDefun (Symbol name) (List params) core
sexprToAST (List [Symbol "lambda", params, core]) = sexprToLambda params core
sexprToAST (List [Symbol "if", cond, trueBody, falseBody]) =
    sexprToCondition cond trueBody falseBody
sexprToAST (List (Symbol s:xs)) = sexprToCall s xs
sexprToAST _ = Nothing

sexprToDefine :: String -> SExpr -> Maybe Ast
sexprToDefine s e = do
  e' <- sexprToAST e
  Just (ADefine {symbol = s, expression = e'})

sexprToCall :: String -> [SExpr] -> Maybe Ast
sexprToCall s args = do
  args' <- traverse sexprToAST args
  Just (ACall {function = s, arguments = args'})

sexprToLambda :: SExpr -> SExpr -> Maybe Ast
sexprToLambda (List params) core@(List _) = do
  paramNames <- traverse extractSymbol params
  functionBody <- sexprToAST core
  Just (AFunction { argumentNames = paramNames, body = functionBody})
sexprToLambda _ _ = Nothing

sexprToDefun :: SExpr -> SExpr -> SExpr -> Maybe Ast
sexprToDefun (Symbol name) (List params) core@(List _) = do
  paramNames <- traverse extractSymbol params
  functionBody <- sexprToAST core
  Just (ADefine { symbol = name, expression =
    ADefun { argumentNames = paramNames, body = functionBody }})
sexprToDefun _ _ _ = Nothing

sexprToCondition :: SExpr -> SExpr -> SExpr -> Maybe Ast
sexprToCondition cond trueBody falseBody = do
  cond' <- sexprToAST cond
  trueBody' <- sexprToAST trueBody
  falseBody' <- sexprToAST falseBody
  Just (ACondition
    { condition = cond', ifTrue = trueBody', ifFalse = falseBody' })

extractSymbol :: SExpr -> Maybe String
extractSymbol (Symbol s) = Just s
extractSymbol _ = Nothing

type Env = [Ast]
type EnvKey = String
type EnvValue = Ast

-- | Insert element in env
envInsert :: Env -> EnvKey -> EnvValue -> Env
envInsert env key value = newKey : deleteBy checkKey newKey env
  where
    checkKey :: Ast -> Ast -> Bool
    checkKey (ADefine sym _) (ADefine sym2 _) = sym == sym2
    checkKey _ _ = False

    newKey :: Ast
    newKey = ADefine { symbol = key, expression = value}

-- | Check if element is in env
envLookup :: Env -> EnvKey -> Maybe EnvValue
envLookup env key = find checkKey env >>= Just . expression
  where
    checkKey :: Ast -> Bool
    checkKey (ADefine sym _) = sym == key
    checkKey _ = False

evalASTCondition :: Env -> Ast -> EvalResult (Env, Ast)
evalASTCondition env (ACondition condExpr thenExpr elseExpr) = do
  (condEnv, condVal) <- evalAST env condExpr
  case condVal of
    ABoolean True -> evalAST condEnv thenExpr
    ABoolean False -> evalAST condEnv elseExpr
    other -> throwEvalError "Condition must evaluate to a boolean value"
              [other]
evalASTCondition _ other = throwEvalError "Condition must be a condition"
                          [other]

-- | Evaluate call function of AST
evalASTCall :: Env -> Ast -> EvalResult Ast
evalASTCall env (ACall name args) = case envLookup env name of
  Just (AFunction argNames funcBody) -> evalAST env' funcBody >>= pure . snd
    where env' = foldl (\acc (n',a) -> envInsert acc n' a)
                 env (zip argNames args)
  Just (AInternalFunction (InternalFunction fct)) -> fct args
  Just sym -> throwEvalError ("Symbol in env '" ++ name ++
    "' is not a function.") [sym]
  Nothing -> throwEvalError ("Function '" ++ name ++
    "' not found in env.") []
evalASTCall _ other = throwEvalError "evalASTCall: AST must be a call" [other]

handleASTCall :: Env -> Ast -> EvalResult Ast
handleASTCall env call@(ACall func _) = case evalASTCall env call of
  EvalResult (Left (EvalError str ast)) ->
    throwEvalError (func ++ ": " ++ str) ast
  other -> other
handleASTCall _ other = throwEvalError
                        "handleASTCall: AST must be a call" [other]

-- | Evaluate AST
evalAST :: Env -> Ast -> EvalResult (Env, Ast)
evalAST env (ASymbol sym) = case envLookup env sym of
  Just val -> pure (env, val)
  Nothing -> throwEvalError ("Symbol '" ++ sym ++ "' not found in env") []
evalAST env (ADefine key expr) = do
  (_, evaluated) <- evalAST env expr
  pure (envInsert env key evaluated, evaluated)
evalAST env cond@(ACondition {}) = evalASTCondition env cond
evalAST env (ACall func args) = traverse (evalAST env) args >>=
    handleASTCall env . ACall func . map snd >>= pure . ((,) env )
evalAST env (ADefun {argumentNames = argNames, body = funcBody}) =
    pure (env, AFunction argNames funcBody)
evalAST env ast = pure (env, ast)
