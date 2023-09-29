module Ast where

import Data.List
import Data.Maybe

data SExpr = Number Int
    | Symbol String
    | List [SExpr]
    deriving (Show, Eq)

getSymbol :: SExpr -> Maybe String
getSymbol (Symbol string) = Just string
getSymbol _ = Nothing

getInteger :: SExpr -> Maybe Int
getInteger (Number int) = Just int
getInteger _ = Nothing

getList :: SExpr -> Maybe SExpr
getList (List list) = Just (List list)
getList _ = Nothing

printTree :: SExpr -> Maybe String
printTree (Number numba) = Just ("a Number '" ++ show numba ++ "' ")
printTree (Symbol simba) = Just ("a Symbol '" ++ simba ++ "' ")
printTree (List []) = Just "an empty List"
printTree (List exprs) = Just ("a List with " ++ formatList exprs)
  where
    formatList :: [SExpr] -> String
    formatList [] = ""
    formatList (x:xs) = formatSExpr x ++ formatList xs

    formatSExpr :: SExpr -> String
    formatSExpr expr = case printTree expr of
      Just s -> s
      Nothing -> "unknown SExpr"


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
        | AInternalFunction { haskellFunction :: InternalFunction }
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


sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Number n) = Just (ANumber n)
sexprToAST (Symbol s) = Just (ASymbol s)
sexprToAST (List [Symbol "define", Symbol s, e]) = case sexprToAST e of
    Just e' -> Just (ADefine {symbol = s, expression = e'})
    Nothing -> Nothing
sexprToAST (List [Symbol s, arg1, arg2]) = case ((sexprToAST arg1), (sexprToAST arg2)) of
    ((Just arg1'), (Just arg2')) -> Just (ACall {function = s, arguments = [arg1', arg2']})
    (_, _) -> Nothing
sexprToAST (List [Symbol "defun", Symbol name, List params, List core]) =
    let
        paramNames = map (\(Symbol p) -> p) params
        functionBodyMaybe = sexprToAST (List core)
    in
    case functionBodyMaybe of
        Just functionBody -> Just (ADefine { symbol = name, expression = ADefun { argumentNames = paramNames, body = functionBody }})
        Nothing -> Nothing
sexprToAST _ = Nothing

type Env = [Ast]
type EnvKey = String
type EnvValue = Ast

envInsert :: Env -> EnvKey -> EnvValue -> Env
envInsert env key value = newKey : deleteBy checkKey newKey env
  where
    checkKey :: Ast -> Ast -> Bool
    checkKey (ADefine sym _) (ADefine sym2 _) = sym == sym2
    checkKey _ _ = False

    newKey :: Ast
    newKey = ADefine { symbol = key, expression = value}

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
    other -> EvalResult (Left (EvalError "Condition must evaluate to a boolean value" [other]))
evalASTCondition _ other =  EvalResult (Left (EvalError "Condition must be a condition" [other]))

internalEnv :: Env
internalEnv = [
    ADefine "+" (AInternalFunction evalAddition),
    ADefine "-" (AInternalFunction evalSoustraction),
    ADefine "*" (AInternalFunction evalMultiplication),
    ADefine "div" (AInternalFunction evalDivision),
    ADefine "mod" (AInternalFunction evalModulo),
    ADefine ">" (AInternalFunction evalGreaterThan)
  ]

evalASTCall :: Env -> Ast -> EvalResult Ast
evalASTCall env (ACall name args) = case envLookup env name of
  Just (AFunction argNames body) -> evalAST env' body >>= pure . snd
    where env' = foldl (\acc (name, arg) -> envInsert acc name arg) env (zip argNames args)
  Just (AInternalFunction (InternalFunction fct)) -> fct args
  Just sym -> EvalResult (Left (EvalError ("Symbol in env '" ++ name ++ "' is not a function.") [sym]))
  Nothing -> EvalResult (Left (EvalError ("Function '" ++ name ++ "' not found in env.") []))
evalASTCall _ other = EvalResult (Left (EvalError "evalASTCall: AST must be a call" [other]))

evalAST :: Env -> Ast -> EvalResult (Env, Ast)
evalAST env (ASymbol sym) = case envLookup env sym of
  Just val -> pure (env, val)
  Nothing -> EvalResult (Left (EvalError ("Symbol '" ++ sym ++ "' not found in env") []))
evalAST env (ADefine key expr) = do
  (_, evaluated) <- evalAST env expr
  pure (envInsert env key evaluated, evaluated)
evalAST env cond@(ACondition {}) = evalASTCondition env cond
evalAST env (ACall func args) = traverse (evalAST env) args >>= evalASTCall env . ACall func . map snd >>= pure . ((,) env )
evalAST env (ADefun argNames body) = pure (env, AFunction argNames body)
evalAST env ast = pure (env, ast)

-- Evaluate addition
evalAddition :: InternalFunction
evalAddition = InternalFunction $ \args -> pure $ ANumber (sumNumbers args)

sumNumbers :: [Ast] -> Int
sumNumbers = sum . map getNumberValue


-- Evaluate multiplication
evalMultiplication :: InternalFunction
evalMultiplication = InternalFunction $ \args -> pure $ ANumber (productNumbers args)

productNumbers :: [Ast] -> Int
productNumbers = product . map getNumberValue


-- Evaluate soustraction
evalSoustraction :: InternalFunction
evalSoustraction = InternalFunction $ \args -> pure $ ANumber (subtractNumbers args)

subtractAll :: Num a => [a] -> a
subtractAll [] = error "Empty list"
subtractAll (x:xs) = foldl (-) x xs

subtractNumbers :: [Ast] -> Int
subtractNumbers = subtractAll . map getNumberValue


-- Evaluate division
evalDivision :: InternalFunction
evalDivision = InternalFunction $ \args -> pure $ ANumber (diviseNumbers args)

divideAll :: [Int] -> Int
divideAll [] = error "Empty list"
divideAll (x:xs) = foldl div x xs

diviseNumbers :: [Ast] -> Int
diviseNumbers = divideAll . map getNumberValue


-- Evaluate modulo
evalModulo :: InternalFunction
evalModulo = InternalFunction $ \args -> pure $ ANumber (moduloNumbers args)

moduloAll :: [Int] -> Int
moduloAll [] = error "Empty list"
moduloAll (x:xs) = foldl mod x xs

moduloNumbers :: [Ast] -> Int
moduloNumbers = moduloAll . map getNumberValue



getNumberValue :: Ast -> Int
getNumberValue (ANumber n) = n
getNumberValue _ = error "Tried to get the number value of a non-number AST node"


-- Evaluate greater than
evalGreaterThan :: InternalFunction
evalGreaterThan = InternalFunction $ \args -> pure $ ABoolean (greaterThan args)

greaterThan :: [Ast] -> Bool
greaterThan [] = error "Empty list"
greaterThan [_] = error "List with only one element"
greaterThan [ANumber x, ANumber y] = x > y
greaterThan _ = error "Invalid number of arguments"
