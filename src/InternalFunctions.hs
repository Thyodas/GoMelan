{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module InternalFunctions (internalEnv) where

import Ast (Ast(..), InternalFunction(..), EvalResult(..), Env,
    throwEvalError)

calculationList :: Env
calculationList = [
    ADefine "+" (AInternalFunction $ InternalFunction evalAddition),
    ADefine "-" (AInternalFunction $ InternalFunction evalSoustraction),
    ADefine "*" (AInternalFunction $ InternalFunction evalMultiplication),
    ADefine "div" (AInternalFunction $ InternalFunction evalDivision),
    ADefine "mod" (AInternalFunction $ InternalFunction evalModulo)
  ]

compList :: Env
compList = [
    ADefine ">" (AInternalFunction $ InternalFunction evalGreaterThan),
    ADefine "<" (AInternalFunction $ InternalFunction evalLessThan),
    ADefine ">=" (AInternalFunction $ InternalFunction evalGreaterEqual),
    ADefine "<=" (AInternalFunction $ InternalFunction evalLowerEqual),
    ADefine "=" (AInternalFunction $ InternalFunction evalEqual)
  ]

compListTwo :: Env
compListTwo = [
  ADefine "eq?" (AInternalFunction $ InternalFunction evalEqual),
    ADefine "eq" (AInternalFunction $ InternalFunction evalEqual),
    ADefine "and" (AInternalFunction $ InternalFunction evalAnd),
    ADefine "or" (AInternalFunction $ InternalFunction evalOr),
    ADefine "not" (AInternalFunction $ InternalFunction evalNot)
  ]

internalEnv :: Env
internalEnv = calculationList ++ compList ++ compListTwo

-- Evaluate not
evalNot :: [Ast] -> EvalResult Ast
evalNot [ABoolean x] = EvalResult (Right (ABoolean (not x)))
evalNot other = throwEvalError "invalid arguments" other

-- Evaluate and
evalAnd :: [Ast] -> EvalResult Ast
evalAnd [ABoolean x, ABoolean y] = EvalResult $ Right $ ABoolean $ x && y
evalAnd other = throwEvalError "invalid arguments" other

-- Evaluate or
evalOr :: [Ast] -> EvalResult Ast
evalOr [ABoolean x, ABoolean y] = EvalResult $ Right $ ABoolean $ x || y
evalOr other = throwEvalError "invalid arguments" other

-- Evaluate addition
evalAddition :: [Ast] -> EvalResult Ast
evalAddition el = traverse getNumberValue el >>= EvalResult . Right . ANumber
    . sum

-- Evaluate multiplication
evalMultiplication :: [Ast] -> EvalResult Ast
evalMultiplication el = traverse getNumberValue el >>= EvalResult . Right
    . ANumber . product

-- Evaluate soustraction
evalSoustraction :: [Ast] -> EvalResult Ast
evalSoustraction [] = throwEvalError
    "incorrect argument count in call (-)" []
evalSoustraction el = traverse getNumberValue el >>= EvalResult . Right
    . ANumber . subtractAll

subtractAll :: Num a => [a] -> a
subtractAll [] = error "Should not happen, substractAll called with empty list"
subtractAll (x:xs) = foldl (-) x xs

-- Evaluate division
evalDivision :: [Ast] -> EvalResult Ast
evalDivision [] = throwEvalError
    "incorrect argument count in call" []
evalDivision [_] = throwEvalError
    "incorrect argument count in call" []
evalDivision arr@(_:xs) | any (== ANumber 0) xs = throwEvalError
    "division by zero" arr
evalDivision el = traverse getNumberValue el >>= EvalResult . Right
    . ANumber . divideAll

divideAll :: [Int] -> Int
divideAll [] = error "Should not happen, divideAll called with empty list"
divideAll (x:xs) = foldl div x xs

-- Evaluate modulo
evalModulo :: [Ast] -> EvalResult Ast
evalModulo [] = throwEvalError
    "incorrect argument count in call" []
evalModulo [_] = throwEvalError
    "incorrect argument count in call" []
evalModulo arr@(_:xs) | any (== ANumber 0) xs = throwEvalError
    "modulo by zero" arr
evalModulo el = traverse getNumberValue el >>= EvalResult . Right
    . ANumber . moduloAll

moduloAll :: [Int] -> Int
moduloAll [] = error "Should not happen, moduloAll called with empty list"
moduloAll (x:xs) = foldl mod x xs


getNumberValue :: Ast -> EvalResult Int
getNumberValue (ANumber n) = EvalResult $ Right $ n
getNumberValue element = throwEvalError
  "tried to get the number value of a non-number AST node" [element]


-- Evaluate greater than
evalGreaterThan :: [Ast] -> EvalResult Ast
evalGreaterThan [ANumber x, ANumber y] = EvalResult $ Right $ ABoolean $ x > y
evalGreaterThan args = throwEvalError "invalid arguments" args

-- Evaluate <
evalLessThan :: [Ast] -> EvalResult Ast
evalLessThan [ANumber x, ANumber y] = EvalResult $ Right $ ABoolean $ x < y
evalLessThan args = throwEvalError "invalid arguments" args

-- Evaluate equal
evalEqual :: [Ast] -> EvalResult Ast
evalEqual [ANumber x, ANumber y] = EvalResult $ Right $ ABoolean $ x == y
evalEqual args = throwEvalError "invalid arguments" args

-- Evaluate lower equal
evalLowerEqual :: [Ast] -> EvalResult Ast
evalLowerEqual args = evalGreaterThan args >>= evalNot . (: [])

-- Evaluate greater equal
evalGreaterEqual :: [Ast] -> EvalResult Ast
evalGreaterEqual args = evalLessThan args >>= evalNot . (: [])
