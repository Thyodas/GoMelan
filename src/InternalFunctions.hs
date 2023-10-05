{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- InternalFunctions
-}

module InternalFunctions (internalEnv) where

import Ast (Ast(..), InternalFunction(..), Env)

calculationList :: Env
calculationList = [
    ADefine "+" (AInternalFunction evalAddition),
    ADefine "-" (AInternalFunction evalSoustraction),
    ADefine "*" (AInternalFunction evalMultiplication),
    ADefine "div" (AInternalFunction evalDivision),
    ADefine "mod" (AInternalFunction evalModulo)
  ]

compList :: Env
compList = [
    ADefine ">" (AInternalFunction evalGreaterThan),
    ADefine "<" (AInternalFunction evalLessThan),
    ADefine ">=" (AInternalFunction evalGreaterEqual),
    ADefine "<=" (AInternalFunction evalLowerEqual),
    ADefine "=" (AInternalFunction evalEqual)
  ]

compListTwo :: Env
compListTwo = [
  ADefine "eq?" (AInternalFunction evalEqual),
    ADefine "eq" (AInternalFunction evalEqual),
    ADefine "and" (AInternalFunction evalAnd),
    ADefine "or" (AInternalFunction evalOr),
    ADefine "not" (AInternalFunction evalNot)
  ]

internalEnv :: Env
internalEnv = calculationList ++ compList ++ compListTwo

-- Evaluate not
evalNot :: InternalFunction
evalNot = InternalFunction $ \args -> pure $ ABoolean (notBoolean args)

notBoolean :: [Ast] -> Bool
notBoolean [] = error "Empty list"
notBoolean [ABoolean x] = not x
notBoolean _ = error "Invalid number of arguments"

-- Evaluate and
evalAnd :: InternalFunction
evalAnd = InternalFunction $ \args -> pure $ ABoolean (andBooleans args)

andBooleans :: [Ast] -> Bool
andBooleans [] = error "Empty list"
andBooleans [_] = error "List with only one element"
andBooleans [ABoolean x, ABoolean y] = x && y
andBooleans _ = error "Invalid number of arguments"

-- Evaluate or
evalOr :: InternalFunction
evalOr = InternalFunction $ \args -> pure $ ABoolean (orBooleans args)

orBooleans :: [Ast] -> Bool
orBooleans [] = error "Empty list"
orBooleans [_] = error "List with only one element"
orBooleans [ABoolean x, ABoolean y] = x || y
orBooleans _ = error "Invalid number of arguments"

-- Evaluate addition
evalAddition :: InternalFunction
evalAddition = InternalFunction $ \args -> pure $ ANumber (sumNumbers args)

sumNumbers :: [Ast] -> Int
sumNumbers = sum . map getNumberValue


-- Evaluate multiplication
evalMultiplication :: InternalFunction
evalMultiplication = InternalFunction $ \args -> pure $ ANumber
  (productNumbers args)

productNumbers :: [Ast] -> Int
productNumbers = product . map getNumberValue


-- Evaluate soustraction
evalSoustraction :: InternalFunction
evalSoustraction = InternalFunction $ \args -> pure $ ANumber
  (subtractNumbers args)

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
getNumberValue _ = error
  "Tried to get the number value of a non-number AST node"


-- Evaluate greater than
evalGreaterThan :: InternalFunction
evalGreaterThan = InternalFunction $ \args -> pure $
  ABoolean (greaterThan args)

greaterThan :: [Ast] -> Bool
greaterThan [] = error "Empty list"
greaterThan [_] = error "List with only one element"
greaterThan [ANumber x, ANumber y] = x > y
greaterThan _ = error "Invalid number of arguments"

-- Evaluate <
evalLessThan :: InternalFunction
evalLessThan = InternalFunction $ \args -> pure $ ABoolean (lessThan args)

lessThan :: [Ast] -> Bool
lessThan [] = error "Empty list"
lessThan [_] = error "List with only one element"
lessThan [ANumber x, ANumber y] = x < y
lessThan _ = error "Invalid number of arguments"

-- Evaluate equal
evalEqual :: InternalFunction
evalEqual = InternalFunction $ \args -> pure $ ABoolean (equal args)

equal :: [Ast] -> Bool
equal [] = error "Empty list"
equal [_] = error "List with only one element"
equal [ANumber x, ANumber y] = x == y
equal _ = error "Invalid number of arguments"

-- Evaluate lower equal
evalLowerEqual :: InternalFunction
evalLowerEqual = InternalFunction $ \args -> pure $ ABoolean
  (not $ greaterThan args)

-- Evaluate greater equal
evalGreaterEqual :: InternalFunction
evalGreaterEqual = InternalFunction $ \args -> pure $ ABoolean
  (not $ lessThan args)
