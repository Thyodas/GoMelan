{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- Spec
-}

import Test.HUnit
import System.Exit
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
