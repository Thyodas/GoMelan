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

evalAST :: Ast -> Maybe Ast
evalAST (ANumber n) = Just (ANumber n)
evalAST (ASymbol _) = Nothing
evalAST (ADefine _ expr) = evalAST expr
evalAST (ACall "+" args) = evalAddition args
evalAST (ACall "-" args) = evalSoustraction args
evalAST (ACall "*" args) = evalMultiplication args
evalAST (ACall "/" args) = evalDivision args
evalAST (ACall "%" args) = evalModulo args
evalAST _ = Nothing

-- Evaluate addition
evalAddition :: [Ast] -> Maybe Ast
evalAddition args = case traverse evalAST args of
  Just argValues -> Just (ANumber (sumNumbers argValues))
  Nothing -> Nothing

sumNumbers :: [Ast] -> Int
sumNumbers = sum . map getNumberValue


-- Evaluate multiplication
evalMultiplication :: [Ast] -> Maybe Ast
evalMultiplication args = case traverse evalAST args of
  Just argValues -> Just (ANumber (productNumbers argValues))
  Nothing -> Nothing

productNumbers :: [Ast] -> Int
productNumbers = product . map getNumberValue


-- Evaluate soustraction
evalSoustraction :: [Ast] -> Maybe Ast
evalSoustraction args = case traverse evalAST args of
  Just argValues -> Just (ANumber (subtractNumbers argValues))
  Nothing -> Nothing

subtractAll :: Num a => [a] -> a
subtractAll [] = error "Empty list"
subtractAll (x:xs) = foldl (-) x xs

subtractNumbers :: [Ast] -> Int
subtractNumbers = subtractAll . map getNumberValue


-- Evaluate division
evalDivision :: [Ast] -> Maybe Ast
evalDivision args = case traverse evalAST args of
  Just argValues -> Just (ANumber (diviseNumbers argValues))
  Nothing -> Nothing

divideAll :: [Int] -> Int
divideAll [] = error "Empty list"
divideAll (x:xs) = foldl div x xs

diviseNumbers :: [Ast] -> Int
diviseNumbers = divideAll . map getNumberValue


-- Evaluate modulo
evalModulo :: [Ast] -> Maybe Ast
evalModulo args = case traverse evalAST args of
  Just argValues -> Just (ANumber (moduloNumbers argValues))
  Nothing -> Nothing

moduloAll :: [Int] -> Int
moduloAll [] = error "Empty list"
moduloAll (x:xs) = foldl mod x xs

moduloNumbers :: [Ast] -> Int
moduloNumbers = moduloAll . map getNumberValue



getNumberValue :: Ast -> Int
getNumberValue (ANumber n) = n
getNumberValue _ = error "Tried to get the number value of a non-number AST node"
