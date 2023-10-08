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
   EvalResult(..), SExpr(..), sexprToAST, InternalFunction(..), sexprToLambda,
   sexprToDefun, extractSymbol, evalASTCall, EvalError(..), evalASTCondition)

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

testLambdaSexprToAST :: Test
testLambdaSexprToAST = "testLambdaSexprToAST" ~: do
    let input = List [Symbol "define", Symbol "add", List [Symbol "lambda", List [Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]]
    let expected = ADefine {symbol = "add", expression = AFunction {argumentNames = ["a","b"], body = ACall {function = "+", arguments = [ASymbol "a",ASymbol "b"]}}}
    assertEqual "should parse lambda expression" expected (fromJust (sexprToAST input))

testDefineSexprToAST :: Test
testDefineSexprToAST = "sexprDefineToAST" ~: do
    let input = List [Symbol "define", List [Symbol "add", Symbol "a", Symbol "b"], List [Symbol "+", Symbol "a", Symbol "b"]]
    let expected = Just (ADefine {symbol = "add", expression = ADefun {argumentNames = ["a","b"], body = ACall {function = "+", arguments = [ASymbol "a",ASymbol "b"]}}})
    assertEqual "should parse define expression" expected (sexprToAST input)

testDefunSexprToAST :: Test
testDefunSexprToAST = "sexprDefunToAST" ~: do
  let input = List [Symbol "defun", Symbol "add", List [Symbol "a", Symbol "b"], List [Symbol "*", Symbol "a", Symbol "b"]]
  let expected = Just (ADefine { symbol = "add", expression = ADefun { argumentNames = ["a", "b"], body = ACall { function = "*", arguments = [ASymbol "a", ASymbol "b"] } }})
  assertEqual "should parse defun expression" expected (sexprToAST input)

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

testSexprToCondition :: Test
testSexprToCondition = TestCase $ assertEqual "Sexpr to condition" expected result
    where
        result = sexprToAST (List [Symbol "if", Symbol "x", Symbol "y", Symbol "z"])
        expected = Just (ACondition {
                condition = ASymbol "x",
                ifTrue = ASymbol "y",
                ifFalse = ASymbol "z"
            })

testShowSExpr :: Test
testShowSExpr = TestCase $ assertEqual "Show SExpr" expected result
    where
        result = show (List [Symbol "if", Symbol "x", Symbol "y", Symbol "z"])
        expected = "List [Symbol \"if\",Symbol \"x\",Symbol \"y\",Symbol \"z\"]"

testEqSExpr :: Test
testEqSExpr = TestList [
    TestCase $ assertEqual "Eq SExpr" expected result,
    TestCase $ assertEqual "Eq SExpr" expected2 result2,
    TestCase $ assertEqual "Eq SExpr" expected3 result3,
    TestCase $ assertEqual "Eq SExpr" expected4 result4,
    TestCase $ assertEqual "Eq SExpr" expected5 result5
    ]
    where
        result = List [] == List []
        expected = True
        result2 = Symbol "z" == Symbol "z"
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

testSexprToAST :: Test
testSexprToAST = TestList
    [ TestCase $ assertEqual "Sexpr to AST" (Just (ADefine "x" (ANumber 42))) result
    , TestCase $ assertEqual "Sexpr to AST" (Just (ABoolean True)) result2
    , TestCase $ assertEqual "Sexpr to AST" Nothing result3
    ]
    where
        result = sexprToAST (List [Symbol "define", Symbol "x", Number 42])
        result2 = sexprToAST (Boolean True)
        result3 = sexprToAST (List [])

testSexprToLambda :: Test
testSexprToLambda = TestCase $ assertEqual "Sexpr to Lambda" expected result
    where
        result = sexprToLambda (List [Symbol "x", Symbol "y"]) (Boolean True)
        expected = Nothing

testsexprToDefun :: Test
testsexprToDefun = TestCase $ assertEqual "Sexpr to defun" expected result
    where
        result = sexprToDefun (List [Symbol "name"]) (List [Symbol "x", Symbol "y"]) (Boolean True)
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
        expected = EvalResult (Left (EvalError ("Symbol in env 'x' is not a function.") [ANumber 42]))

testAstCallWithSymbolNotInEnv :: Test
testAstCallWithSymbolNotInEnv = TestCase $ assertEqual "Ast call" expected result
    where
        result = evalASTCall [] (ACall "y" [])
        expected = EvalResult (Left (EvalError ("Function 'y' not found in env.") []))


testEvalAST :: Test
testEvalAST = TestList
    [ TestCase $ assertEqual "EvalAST - Symbol found" expectedFound resultFound
    , TestCase $ assertEqual "EvalAST - Symbol not found" expectedNotFound resultNotFound
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
        expectedNotFound = EvalResult (Left (EvalError "Symbol 'y' not found in env" []))

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
    testDefunSexprToAST,
    testRecursiveFunction,
    testLambdaSexprToAST,
    testSexprToCondition,
    testShowSExpr,
    testEqSExpr,
    testShowInternalFunction,
    testSexprToAST,
    testSexprToLambda,
    testsexprToDefun,
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
    testDefineSexprToAST
    ]
