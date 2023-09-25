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
import AstTest (astTestList)
import ParserTest (parserTestList)

testList :: Test
testList = TestList [
    astTestList,
    parserTestList
    ]

main :: IO ()
main = do
    x <- runTestTT testList
    if failures x > 0 || errors x > 0 then exitFailure else exitSuccess
