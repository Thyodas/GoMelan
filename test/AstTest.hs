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
import Ast (Env, EnvKey, EnvValue, envInsert, envLookup, Ast(..), evalAST)

testEnv :: Env
testEnv = [
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
testLookupExists = TestCase $ assertEqual "Element exists" (Just (ASymbol "value3")) (envLookup testEnv "key3")

testLookupNotExists :: Test
testLookupNotExists = TestCase $ assertEqual "Element not exists" Nothing (envLookup testEnv "key5")

testAst :: Test
testAst = TestCase $ assertEqual "Ast basic" expected result
    where
        result = evalAST [] (ACall "*" [ACall "+" [ANumber 4, ANumber 3], ANumber 6])
        expected = Just (ANumber 42)

testAstEnv :: Test
testAstEnv = TestCase $ assertEqual "Ast basic env" expected result
    where
        result = evalAST env (ACall "*" [ACall "+" [ASymbol "x", ANumber 3], ASymbol "y"])
        expected = Just (ANumber 42)
        env = [ADefine "x" (ANumber 4), ADefine "y" (ANumber 6)]

testAstConditon :: Test
testAstConditon = TestCase $ assertEqual "Ast condition" expected result
    where
        result = evalAST env (ACondition {
                condition = ASymbol "x",
                ifTrue = ANumber 42,
                ifFalse = ANumber 0
            })
        expected = Just (ANumber 42)
        env = [ADefine "x" (ABoolean True)]

astTestList :: Test
astTestList = TestList [
    testInsert,
    testLookupExists,
    testLookupNotExists,
    testAst,
    testAstEnv,
    testAstConditon
    ]
