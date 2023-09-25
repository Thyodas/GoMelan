{-
-- EPITECH PROJECT, 2023
-- paradigms_seminar [WSL: Ubuntu]
-- File description:
-- ParserTest
-}

module ParserTest (parserTestList) where

import Test.HUnit
import System.Exit
import Data.Maybe
import Test.HUnit.Text
import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Parser (parseAnyChar, parseChar, parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parseInt, Parser)

testParseAnyChar :: Test
testParseAnyChar = TestCase $ assertEqual "parseAnyChar" expected result
    where
        result = parseAnyChar "bca" "abcd"
        expected = Right ('a', "bcd")

testParseAnyCharFail :: Test
testParseAnyCharFail = TestCase $ assertEqual "parseAnyChar failure" expected result
    where
        result = parseAnyChar "bca" "defg"
        expected = Left "Expected one of 'bca' but got 'd'."

testParseOr :: Test
testParseOr = TestCase $ assertEqual "parseOr valid" expected result
    where
        result = parseOr (parseChar 'a') (parseChar 'b') "abcd"
        expected = Right ('a', "bcd")

testParseOrFirstFail :: Test
testParseOrFirstFail = TestCase $ assertEqual "parseOr first failure second success" expected result
    where
        result = parseOr (parseChar 'c') (parseChar 'b') "bcd"
        expected = Right ('b', "cd")

testParseOrBothFail :: Test
testParseOrBothFail = TestCase $ assertEqual "parseOr both failure" expected result
    where
        result = parseOr (parseChar 'c') (parseChar 'b') "defg"
        expected = Left "Expected 'b' but got 'd'."

testParseAnd :: Test
testParseAnd = TestCase $ assertEqual "parseAnd valid" expected result
    where
        result = parseAnd (parseChar 'a') (parseChar 'b') "abcd"
        expected = Right (('a', 'b'), "cd")

testParseAndFirstFail :: Test
testParseAndFirstFail = TestCase $ assertEqual "parseAnd first failure" expected result
    where
        result = parseAnd (parseChar 'c') (parseChar 'b') "bcd"
        expected = Left "Expected 'c' but got 'b'."

testParseAndWith :: Test
testParseAndWith = TestCase $ assertEqual "parseAndWith valid" expected result
    where
        result = parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b') "abcd"
        expected = Right ("ab", "cd")

testParseManyOne :: Test
testParseManyOne = TestCase $ assertEqual "parseMany one" expected result
    where
        result = parseMany (parseChar ' ') "   foobar"
        expected = Right ("   ", "foobar")

testParseManyTwo :: Test
testParseManyTwo = TestCase $ assertEqual "parseMany two" expected result
    where
        result = parseMany (parseChar ' ') "foobar   "
        expected = Right ("", "foobar   ")

testParseSomeValid :: Test
testParseSomeValid = TestCase $ assertEqual "parseSome valid" expected result
    where
        result = parseSome (parseAnyChar ['0'..'9']) "42foobar"
        expected = Right ("42", "foobar")

testParseSomeFail :: Test
testParseSomeFail = TestCase $ assertEqual "parseSome valid" expected result
    where
        result = parseSome (parseAnyChar ['0'..'9']) "foobar42"
        expected = Left "Expected one of '0123456789' but got 'f'."

testParseInt :: Test
testParseInt = TestCase $ assertEqual "parseInt valid" expected result
    where
        result = parseInt "42foobar"
        expected = Right (42, "foobar")

testParseIntTwo :: Test
testParseIntTwo = TestCase $ assertEqual "parseInt valid two" expected result
    where
        result = parseInt "+100"
        expected = Right (100, "")

testParseIntNegative :: Test
testParseIntNegative = TestCase $ assertEqual "parseInt negative" expected result
    where
        result = parseInt "-42foobar"
        expected = Right (-42, "foobar")

parserTestList :: Test
parserTestList = TestList [
    testParseAnyChar,
    testParseAnyCharFail,
    testParseOr,
    testParseOrFirstFail,
    testParseOrBothFail,
    testParseAnd,
    testParseAndFirstFail,
    testParseAndWith,
    testParseManyOne,
    testParseManyTwo,
    testParseSomeValid,
    testParseSomeFail,
    testParseInt,
    testParseIntTwo,
    testParseIntNegative
    ]
