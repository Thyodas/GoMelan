module Ast where

import Data.List

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

data Ast = ADefine { symbol :: String, expression :: Ast }
        | ACall { function :: String, arguments :: [Ast] }
        | ANumber Int
        | ASymbol String
        | AString String
        | ABoolean Bool
    deriving (Show, Eq)

sexprToAST :: SExpr -> Maybe Ast
sexprToAST (Number n) = Just (ANumber n)
sexprToAST (Symbol s) = Just (ASymbol s)
sexprToAST (List [Symbol "define", Symbol s, e]) = case sexprToAST e of
    Just e' -> Just (ADefine {symbol = s, expression = e'})
    Nothing -> Nothing
sexprToAST (List [Symbol s, arg1, arg2]) = case ((sexprToAST arg1), (sexprToAST arg2)) of
    ((Just arg1'), (Just arg2')) -> Just (ACall {function = s, arguments = [arg1', arg2']})
    (_, _) -> Nothing
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

resolveSymbol :: Env -> Ast -> Maybe Ast
resolveSymbol env (ASymbol sym) = envLookup env sym
resolveSymbol env other = Just other

handleArgs :: Env -> [Ast] -> Maybe [Ast]
handleArgs env args = (traverse (evalAST env) args) >>= (traverse (resolveSymbol env))

evalAST :: Env -> Ast -> Maybe Ast
evalAST env (ANumber n) = Just (ANumber n)
evalAST env (ASymbol _) = Nothing
evalAST env (ADefine key expr) = evalAST env expr -- envInsert env key (evalAST expr)
evalAST env (ACall "+" args) = handleArgs env args >>= evalAddition
evalAST env (ACall "-" args) = handleArgs env args >>= evalSoustraction
evalAST env (ACall "*" args) = handleArgs env args >>= evalMultiplication
evalAST env (ACall "/" args) = handleArgs env args >>= evalDivision
evalAST env (ACall "%" args) = handleArgs env args >>= evalModulo
evalAST _ _ = Nothing

-- Evaluate addition
evalAddition :: [Ast] -> Maybe Ast
evalAddition args = Just $ ANumber (sumNumbers args)

sumNumbers :: [Ast] -> Int
sumNumbers = sum . map getNumberValue


-- Evaluate multiplication
evalMultiplication :: [Ast] -> Maybe Ast
evalMultiplication args = Just $ ANumber (productNumbers args)

productNumbers :: [Ast] -> Int
productNumbers = product . map getNumberValue


-- Evaluate soustraction
evalSoustraction :: [Ast] -> Maybe Ast
evalSoustraction args = Just $ ANumber (subtractNumbers args)

subtractAll :: Num a => [a] -> a
subtractAll [] = error "Empty list"
subtractAll (x:xs) = foldl (-) x xs

subtractNumbers :: [Ast] -> Int
subtractNumbers = subtractAll . map getNumberValue


-- Evaluate division
evalDivision :: [Ast] -> Maybe Ast
evalDivision args = Just $ ANumber (diviseNumbers args)

divideAll :: [Int] -> Int
divideAll [] = error "Empty list"
divideAll (x:xs) = foldl div x xs

diviseNumbers :: [Ast] -> Int
diviseNumbers = divideAll . map getNumberValue


-- Evaluate modulo
evalModulo :: [Ast] -> Maybe Ast
evalModulo args = Just $ ANumber (moduloNumbers args)

moduloAll :: [Int] -> Int
moduloAll [] = error "Empty list"
moduloAll (x:xs) = foldl mod x xs

moduloNumbers :: [Ast] -> Int
moduloNumbers = moduloAll . map getNumberValue



getNumberValue :: Ast -> Int
getNumberValue (ANumber n) = n
getNumberValue _ = error "Tried to get the number value of a non-number AST node"
