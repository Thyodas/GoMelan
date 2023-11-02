{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- Spec
-}

module AstTest (astTestList) where

import Test.HUnit
import Data.Maybe
import InternalFunctions (internalEnv)
import Ast (Env, envInsert, envLookup, Ast(..), evalAST,
   EvalResult(..), GomExpr(..), gomexprToAST, InternalFunction(..), gomexprToLambda,
   gomexprToDefun, extractSymbol, evalASTCall, EvalError(..), evalASTCondition)

testEnv :: Env
testEnv = internalEnv ++ [
    ADefine "key1" (ASymbol "value1"),
    ADefine "key2" (ASymbol "value2"),
    ADefine "key3" (ASymbol "value3"),
    ADefine "key4" (ASymbol "value4")
    ]

testInsert :: Test
testInsert = TestList [
    TestCase $ assertEqual "Element inserted" (newElement : testEnv) (envInsert testEnv "key5" (ASymbol "value5")),
    TestCase $ assertEqual "Element inserted" (newElement2 : testEnv) (envInsert testEnv "key5" (ASymbol "value5"))
    ]
    where
        newElement = ADefine "key5" (ASymbol "value5")
        newElement2 = ADefine "key5" (ASymbol "value5")

-- testInsertCheckKeyFalse :: Test
-- testInsertCheckKeyFalse = TestList [
--     TestCase $ assertEqual "Element not inserted" testEnv (envInsert testEnv "key5" (ASymbol "value5")),
--     TestCase $ assertEqual "Element not inserted" (newElement : testEnv) (envInsert testEnv "key5" (ANumber 42))
--     ]
--     where
--         newElement = ADefine "key5" (ASymbol "value5")

testLookupExists :: Test
testLookupExists = TestCase $ assertEqual "Element exists" (ASymbol "value3") (fromJust (envLookup testEnv "key3"))

testLookupNotExists :: Test
testLookupNotExists = TestCase $ assertEqual "Element not exists" Nothing (envLookup testEnv "key5")

testAst :: Test
testAst = TestCase $ assertEqual "Ast basic" expected result
    where
        result = evalAST internalEnv (ACall "*" [ACall "+" [ANumber 4, ANumber 3], ANumber 6])
        expected =  EvalResult $ Right (internalEnv, ANumber 42)

testAstEnv :: Test
testAstEnv = TestCase $ assertEqual "Ast basic env" expected result
    where
        result = evalAST env (ACall "*" [ACall "+" [ASymbol "x", ANumber 3], ASymbol "y"])
        expected =  EvalResult $ Right (env, ANumber 42)
        env = internalEnv ++ [ADefine "x" (ANumber 4), ADefine "y" (ANumber 6)]

testAstConditon :: Test
testAstConditon = TestCase $ assertEqual "Ast condition" expected result
    where
        result = evalAST env (ACondition {
                condition = ASymbol "x",
                ifTrue = ANumber 42,
                ifFalse = ANumber 0
            })
        expected = EvalResult $ Right (env, ANumber 42)
        env = internalEnv ++ [ADefine "x" (ABoolean True)]

testAstConditionNotBoolean :: Test
testAstConditionNotBoolean = TestCase $ assertEqual "Ast condition" expected result
    where
        result = evalAST env (ACondition {
                condition = ANumber 42,
                ifTrue = ANumber 10,
                ifFalse = ANumber 20
            })
        expected = EvalResult (Left (EvalError "Condition must evaluate to a boolean value" [ANumber 42]))
        env = internalEnv

testAstConditionNotACondition :: Test
testAstConditionNotACondition = TestCase $ assertEqual "Ast condition" expected result
    where
        result = evalASTCondition env (ANumber 42)
        expected = EvalResult (Left (EvalError "Condition must be a condition" [ANumber 42]))
        env = internalEnv

testLambdaGomexprToAST :: Test
testLambdaGomexprToAST = "testLambdaGomexprToAST" ~: do
    let input = List [Identifier "define", Identifier "add", List [Identifier "lambda", List [Identifier "a", Identifier "b"], List [Identifier "+", Identifier "a", Identifier "b"]]]
    let expected = ADefine {symbol = "add", expression = AFunction {argumentNames = ["a","b"], body = ACall {function = "+", arguments = [ASymbol "a",ASymbol "b"]}}}
    assertEqual "should parse lambda expression" expected (fromJust (gomexprToAST input))

testDefineGomexprToAST :: Test
testDefineGomexprToAST = "gomexprDefineToAST" ~: do
    let input = List [Identifier "define", List [Identifier "add", Identifier "a", Identifier "b"], List [Identifier "+", Identifier "a", Identifier "b"]]
    let expected = Just (ADefine {symbol = "add", expression = ADefun {argumentNames = ["a","b"], body = ACall {function = "+", arguments = [ASymbol "a",ASymbol "b"]}}})
    assertEqual "should parse define expression" expected (gomexprToAST input)

testDefunGomexprToAST :: Test
testDefunGomexprToAST = "gomexprDefunToAST" ~: do
  let input = List [Identifier "defun", Identifier "add", List [Identifier "a", Identifier "b"], List [Identifier "*", Identifier "a", Identifier "b"]]
  let expected = Just (ADefine { symbol = "add", expression = ADefun { argumentNames = ["a", "b"], body = ACall { function = "*", arguments = [ASymbol "a", ASymbol "b"] } }})
  assertEqual "should parse defun expression" expected (gomexprToAST input)

testRecursiveFunction :: Test
testRecursiveFunction = TestCase $ assertEqual "Recursive function" expected result
    where
        result = evalAST env (ACall "factorial" [ANumber 5])
        expected = EvalResult $ Right (env, ANumber 120)
        env = internalEnv ++ [ADefine "factorial" (AFunction ["n"] (ACondition {
                condition = ACall ">" [ASymbol "n", ANumber 1],
                ifTrue = ACall "*" [ASymbol "n", ACall "factorial" [ACall "-" [ASymbol "n", ANumber 1]]],
                ifFalse = ANumber 1
            }))]

testGomexprToCondition :: Test
testGomexprToCondition = TestCase $ assertEqual "Gomexpr to condition" expected result
    where
        result = gomexprToAST (List [Identifier "if", Identifier "x", Identifier "y", Identifier "z"])
        expected = Just (ACondition {
                condition = ASymbol "x",
                ifTrue = ASymbol "y",
                ifFalse = ASymbol "z"
            })

testShowGomExpr :: Test
testShowGomExpr = TestCase $ assertEqual "Show GomExpr" expected result
    where
        result = show (List [Identifier "if", Identifier "x", Identifier "y", Identifier "z"])
        expected = "List [Identifier \"if\",Identifier \"x\",Identifier \"y\",Identifier \"z\"]"

testEqGomExpr :: Test
testEqGomExpr = TestList [
    TestCase $ assertEqual "Eq GomExpr" expected result,
    TestCase $ assertEqual "Eq GomExpr" expected2 result2,
    TestCase $ assertEqual "Eq GomExpr" expected3 result3,
    TestCase $ assertEqual "Eq GomExpr" expected4 result4,
    TestCase $ assertEqual "Eq GomExpr" expected5 result5
    ]
    where
        result = List [] == List []
        expected = True
        result2 = Identifier "z" == Identifier "z"
        expected2 = True
        result3 = Number 2 == Number 2
        expected3 = True
        result4 = Boolean True == Boolean True
        expected4 = True
        result5 = Boolean False == Boolean True
        expected5 = False

testShowInternalFunction :: Test
testShowInternalFunction = TestCase $ assertEqual "Show InternalFunction" expected result
    where
        result = show (InternalFunction (\_ -> EvalResult (Right (ANumber 42))))
        expected = "<Internal Function>"

testGomexprToAST :: Test
testGomexprToAST = TestList
    [ TestCase $ assertEqual "Gomexpr to AST" (Just (ADefine "x" (ANumber 42))) result
    , TestCase $ assertEqual "Gomexpr to AST" (Just (ABoolean True)) result2
    , TestCase $ assertEqual "Gomexpr to AST" Nothing result3
    ]
    where
        result = gomexprToAST (List [Identifier "define", Identifier "x", Number 42])
        result2 = gomexprToAST (Boolean True)
        result3 = gomexprToAST (List [])

testGomexprToLambda :: Test
testGomexprToLambda = TestCase $ assertEqual "Gomexpr to Lambda" expected result
    where
        result = gomexprToLambda (List [Identifier "x", Identifier "y"]) (Boolean True)
        expected = Nothing

testgomexprToDefun :: Test
testgomexprToDefun = TestCase $ assertEqual "Gomexpr to defun" expected result
    where
        result = gomexprToDefun (List [Identifier "name"]) (List [Identifier "x", Identifier "y"]) (Boolean True)
        expected = Nothing

testextractSymbol :: Test
testextractSymbol = TestCase $ assertEqual "is not string" expected result
    where
        result = extractSymbol (List [])
        expected = Nothing


testAstCall :: Test
testAstCall = TestCase $ assertEqual "Ast call" expected result
    where
        result = evalASTCall env (ACall "add" [ANumber 4, ANumber 3])
        expected = EvalResult $ Right (ANumber 7)
        env = internalEnv ++ [ADefine "add" (AFunction ["a", "b"] (ACall "+" [ASymbol "a", ASymbol "b"]))]

testAstCallWithNothing :: Test
testAstCallWithNothing = TestCase $ assertEqual "Ast call" expected result
    where
        result = evalASTCall [ASymbol "x"] (ACall "x" [])
        expected = EvalResult (Left (EvalError ("Function 'x' not found in env.") []))

testAstCallWithOther :: Test
testAstCallWithOther = TestCase $ assertEqual "Ast call" expected result
    where
        result = evalASTCall [ASymbol "x"] (ASymbol "x")
        expected = EvalResult (Left (EvalError ("evalASTCall: AST must be a call") [ASymbol "x"]))

testAstCallWithSymbolNotAFunction :: Test
testAstCallWithSymbolNotAFunction = TestCase $ assertEqual "Ast call" expected result
    where
        result = evalASTCall [ADefine "x" (ANumber 42)] (ACall "x" [])
        expected = EvalResult (Left (EvalError ("Identifier in env 'x' is not a function.") [ANumber 42]))

testAstCallWithSymbolNotInEnv :: Test
testAstCallWithSymbolNotInEnv = TestCase $ assertEqual "Ast call" expected result
    where
        result = evalASTCall [] (ACall "y" [])
        expected = EvalResult (Left (EvalError ("Function 'y' not found in env.") []))


testEvalAST :: Test
testEvalAST = TestList
    [ TestCase $ assertEqual "EvalAST - Identifier found" expectedFound resultFound
    , TestCase $ assertEqual "EvalAST - Identifier not found" expectedNotFound resultNotFound
    , TestCase $ assertEqual "EvalAST - Define expression" expectedDefine resultDefine
    , TestCase $ assertEqual "EvalAST - Condition expression" expectedCondition resultCondition
    , TestCase $ assertEqual "EvalAST - Defun expression" expectedDefun resultDefun
    , TestCase $ assertEqual "EvalAST - Other expression" expectedOther resultOther
    ]
    where
        env = [ADefine "x" (ANumber 42)]
        resultFound = evalAST env (ASymbol "x")
        expectedFound = EvalResult (Right (env, ANumber 42))

        resultNotFound = evalAST env (ASymbol "y")
        expectedNotFound = EvalResult (Left (EvalError "Identifier 'y' not found in env" []))

        resultDefine = evalAST env (ADefine "y" (ANumber 10))
        expectedDefine = EvalResult (Right (envInsert env "y" (ANumber 10), ANumber 10))

        resultCondition = evalAST env (ACondition (ABoolean True) (ANumber 1) (ANumber 2))
        expectedCondition = EvalResult (Right (env, ANumber 1))

        resultDefun = evalAST env (ADefun ["a", "b"] (ACall "+" [ASymbol "a", ASymbol "b"]))
        expectedDefun = EvalResult (Right (env, AFunction ["a", "b"] (ACall "+" [ASymbol "a", ASymbol "b"])))

        resultOther = evalAST env (ANumber 42)
        expectedOther = EvalResult (Right (env, ANumber 42))




astTestList :: Test
astTestList = TestList [
    testInsert,
    testLookupExists,
    testLookupNotExists,
    testAst,
    testAstEnv,
    testAstConditon,
    testDefunGomexprToAST,
    testRecursiveFunction,
    testLambdaGomexprToAST,
    testGomexprToCondition,
    testShowGomExpr,
    testEqGomExpr,
    testShowInternalFunction,
    testGomexprToAST,
    testGomexprToLambda,
    testgomexprToDefun,
    testextractSymbol,
    testAstCall,
    testAstCallWithNothing,
    testAstCallWithOther,
    testEvalAST,
    testAstCallWithSymbolNotAFunction,
    testAstCallWithSymbolNotInEnv,
    testAstConditionNotBoolean,
    testAstConditionNotACondition,
    -- testInsertCheckKeyFalse,
    testDefineGomexprToAST
    ]
