{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- Spec
-}

module AstTest (astTestList) where

import Test.HUnit
import System.Exit
import Data.Maybe
import Test.HUnit.Text
import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Ast (Env, EnvKey, EnvValue, envInsert, envLookup, Ast(..), evalAST,
   EvalResult(..), internalEnv, SExpr(..), sexprToAST)

testEnv :: Env
testEnv = internalEnv ++ [
    ADefine "key1" (ASymbol "value1"),
    ADefine "key2" (ASymbol "value2"),
    ADefine "key3" (ASymbol "value3"),
    ADefine "key4" (ASymbol "value4")
    ]

testInsert :: Test
testInsert = TestCase $ assertEqual "Element inserted" (newElement : testEnv) (envInsert testEnv "key5" (ASymbol "value5"))
    where
        newElement = ADefine "key5" (ASymbol "value5")

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

testSexprToAST :: Test
testSexprToAST = "sexprToAST" ~: do
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

astTestList :: Test
astTestList = TestList [
    testInsert,
    testLookupExists,
    testLookupNotExists,
    testAst,
    testAstEnv,
    testAstConditon,
    testSexprToAST,
    testRecursiveFunction
    ]
