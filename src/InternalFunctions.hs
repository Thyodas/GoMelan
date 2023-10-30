{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module InternalFunctions (internalEnv) where

import Ast (GomAST(..), InternalFunction(..), EvalResult(..), Env,
    throwEvalError)

calculationList :: Env
calculationList = [
        ("+", AGomInternalFunction $ InternalFunction evalAddition),
        ("-", AGomInternalFunction $ InternalFunction evalSoustraction),
        ("*", AGomInternalFunction $ InternalFunction evalMultiplication),
        ("/", AGomInternalFunction $ InternalFunction evalDivision),
        ("%", AGomInternalFunction $ InternalFunction evalModulo)
    ]

compList :: Env
compList = [
        (">", AGomInternalFunction $ InternalFunction evalGreaterThan),
        ("<", AGomInternalFunction $ InternalFunction evalLessThan),
        (">=", AGomInternalFunction $ InternalFunction evalGreaterEqual),
        ("<=", AGomInternalFunction $ InternalFunction evalLowerEqual),
        ("==", AGomInternalFunction $ InternalFunction evalEqual),
        ("!=", AGomInternalFunction $ InternalFunction evalNotEqual)
    ]

compListTwo :: Env
compListTwo = [
        ("&&", AGomInternalFunction $ InternalFunction evalAnd),
        ("||", AGomInternalFunction $ InternalFunction evalOr),
        ("!", AGomInternalFunction $ InternalFunction evalNot)
    ]

internalEnv :: Env
internalEnv = calculationList ++ compList ++ compListTwo

-- Evaluate not
evalNot :: [GomAST] -> EvalResult GomAST
evalNot [AGomBooleanLiteral x] = pure $ AGomBooleanLiteral (not x)
evalNot _ = throwEvalError "invalid arguments" []

-- Evaluate and
evalAnd :: [GomAST] -> EvalResult GomAST
evalAnd [AGomBooleanLiteral x, AGomBooleanLiteral y] = pure $
    AGomBooleanLiteral $ x && y
evalAnd _ = throwEvalError "invalid arguments" []

-- Evaluate or
evalOr :: [GomAST] -> EvalResult GomAST
evalOr [AGomBooleanLiteral x, AGomBooleanLiteral y] = pure $
    AGomBooleanLiteral $ x || y
evalOr _ = throwEvalError "invalid arguments" []

-- Evaluate addition
evalAddition :: [GomAST] -> EvalResult GomAST
evalAddition el = traverse getNumberValue el >>= pure . AGomNumber . sum

-- Evaluate multiplication
evalMultiplication :: [GomAST] -> EvalResult GomAST
evalMultiplication el = traverse getNumberValue el >>= pure . AGomNumber
    . product

-- Evaluate soustraction
evalSoustraction :: [GomAST] -> EvalResult GomAST
evalSoustraction [] = throwEvalError
    "incorrect argument count in call (-)" []
evalSoustraction el = traverse getNumberValue el >>= pure . AGomNumber
    . subtractAll

subtractAll :: Num a => [a] -> a
subtractAll [] = error "Should not happen, substractAll called with empty list"
subtractAll (x:xs) = foldl (-) x xs

-- Evaluate division
evalDivision :: [GomAST] -> EvalResult GomAST
evalDivision [] = throwEvalError
    "incorrect argument count in call" []
evalDivision [_] = throwEvalError
    "incorrect argument count in call" []
evalDivision (_:xs) | AGomNumber 0 `elem` xs = throwEvalError
    "division by zero" []
evalDivision el = traverse getNumberValue el >>= pure . AGomNumber . divideAll

divideAll :: [Int] -> Int
divideAll [] = error "Should not happen, divideAll called with empty list"
divideAll (x:xs) = foldl div x xs

-- Evaluate modulo
evalModulo :: [GomAST] -> EvalResult GomAST
evalModulo [] = throwEvalError
    "incorrect argument count in call" []
evalModulo [_] = throwEvalError
    "incorrect argument count in call" []
evalModulo (_:xs) | AGomNumber 0 `elem` xs = throwEvalError
    "modulo by zero" []
evalModulo el = traverse getNumberValue el >>= pure . AGomNumber . moduloAll

moduloAll :: [Int] -> Int
moduloAll [] = error "Should not happen, moduloAll called with empty list"
moduloAll (x:xs) = foldl mod x xs


getNumberValue :: GomAST -> EvalResult Int
getNumberValue (AGomNumber n) = pure n
getNumberValue _ = throwEvalError
  "tried to get the number value of a non-number GomAST node" []


-- Evaluate greater than
evalGreaterThan :: [GomAST] -> EvalResult GomAST
evalGreaterThan [AGomNumber x, AGomNumber y] = pure $ AGomBooleanLiteral $ x > y
evalGreaterThan _ = throwEvalError "invalid arguments" []

-- Evaluate <
evalLessThan :: [GomAST] -> EvalResult GomAST
evalLessThan [AGomNumber x, AGomNumber y] = pure $ AGomBooleanLiteral $ x < y
evalLessThan _ = throwEvalError "invalid arguments" []

-- Evaluate equal
evalEqual :: [GomAST] -> EvalResult GomAST
evalEqual [AGomNumber x, AGomNumber y] = pure $ AGomBooleanLiteral $ x == y
evalEqual _ = throwEvalError "invalid arguments" []

-- Evaluate not equal
evalNotEqual :: [GomAST] -> EvalResult GomAST
evalNotEqual [AGomNumber x, AGomNumber y] = pure $ AGomBooleanLiteral $ x /= y
evalNotEqual _ = throwEvalError "invalid arguments" []

-- Evaluate lower equal
evalLowerEqual :: [GomAST] -> EvalResult GomAST
evalLowerEqual args = evalGreaterThan args >>= evalNot . (: [])

-- Evaluate greater equal
evalGreaterEqual :: [GomAST] -> EvalResult GomAST
evalGreaterEqual args = evalLessThan args >>= evalNot . (: [])
