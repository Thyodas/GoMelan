{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- Spec
-}

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

testAST :: Test
testAST = TestCase $ assertEqual "AST" expected result
    where
        result = evalAST [] (ACall "*" [ACall "+" [ANumber 4, ANumber 3], ANumber 6])
        expected = Just (ANumber 42)

testList :: Test
testList = TestList [
    testInsert,
    testLookupExists,
    testLookupNotExists,
    testAST
    ]

main :: IO ()
main = do
    x <- runTestTT testList
    if failures x > 0 || errors x > 0 then exitFailure else exitSuccess