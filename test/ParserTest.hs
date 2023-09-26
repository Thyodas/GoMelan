module ParserTest (parserTestList) where

import Test.HUnit
import Parser (parseAnyChar, parseChar, parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parseInt, parsePair, parseList, Parser, runParser)

testParseAnyChar :: Test
testParseAnyChar = TestCase $ assertEqual "parseAnyChar" expected result
    where
        result = runParser (parseAnyChar "bca") "abcd"
        expected = Right ('a', "bcd")

testParseAnyCharFailEmpty :: Test
testParseAnyCharFailEmpty = TestCase $ assertEqual "parseAnyChar empty" expected result
    where
        result = runParser (parseAnyChar "bca") ""
        expected = Left "Expected one of 'bca' but got empty string."

testParseAnyCharFail :: Test
testParseAnyCharFail = TestCase $ assertEqual "parseAnyChar failure" expected result
    where
        result = runParser (parseAnyChar "bca") "defg"
        expected = Left "Expected one of 'bca' but got 'd'."

testParseOr :: Test
testParseOr = TestCase $ assertEqual "parseOr valid" expected result
    where
        result = runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Right ('a', "bcd")

testParseOrFirstFail :: Test
testParseOrFirstFail = TestCase $ assertEqual "parseOr first failure second success" expected result
    where
        result = runParser (parseOr (parseChar 'c') (parseChar 'b')) "bcd"
        expected = Right ('b', "cd")

testParseOrBothFail :: Test
testParseOrBothFail = TestCase $ assertEqual "parseOr both failure" expected result
    where
        result = runParser (parseOr (parseChar 'c') (parseChar 'b')) "defg"
        expected = Left "Expected 'b' but got 'd'."

testParseAnd :: Test
testParseAnd = TestCase $ assertEqual "parseAnd valid" expected result
    where
        result = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Right (('a', 'b'), "cd")

testParseAndFirstFail :: Test
testParseAndFirstFail = TestCase $ assertEqual "parseAnd first failure" expected result
    where
        result = runParser (parseAnd (parseChar 'c') (parseChar 'b')) "bcd"
        expected = Left "Expected 'c' but got 'b'."

testParseAndWith :: Test
testParseAndWith = TestCase $ assertEqual "parseAndWith valid" expected result
    where
        result = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Right ("ab", "cd")

testParseManyOne :: Test
testParseManyOne = TestCase $ assertEqual "parseMany one" expected result
    where
        result = runParser (parseMany (parseChar ' ')) "   foobar"
        expected = Right ("   ", "foobar")

testParseManyTwo :: Test
testParseManyTwo = TestCase $ assertEqual "parseMany two" expected result
    where
        result = runParser (parseMany (parseChar ' ')) "foobar   "
        expected = Right ("", "foobar   ")

testParseSomeValid :: Test
testParseSomeValid = TestCase $ assertEqual "parseSome valid" expected result
    where
        result = runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"
        expected = Right ("42", "foobar")

testParseSomeFail :: Test
testParseSomeFail = TestCase $ assertEqual "parseSome valid" expected result
    where
        result = runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"
        expected = Left "Expected one of '0123456789' but got 'f'."

testParseInt :: Test
testParseInt = TestCase $ assertEqual "parseInt valid" expected result
    where
        result = runParser (parseInt :: Parser Int) "42foobar"
        expected = Right (42, "foobar")

testParseIntTwo :: Test
testParseIntTwo = TestCase $ assertEqual "parseInt valid two" expected result
    where
        result = runParser (parseInt :: Parser Int) "+100"
        expected = Right (100, "")

testParseIntNegative :: Test
testParseIntNegative = TestCase $ assertEqual "parseInt negative" expected result
    where
        result = runParser (parseInt :: Parser Int) "-42foobar"
        expected = Right (-42, "foobar")

testParsePair :: Test
testParsePair = TestCase $ assertEqual "parsePair valid" expected result
    where
        result = runParser (parsePair parseInt) "(123 456)foo bar"
        expected = Right ((123, 456), "foo bar")

testParseList :: Test
testParseList = TestCase $ assertEqual "parseList valid" expected result
    where
        result = runParser (parseList parseInt) "(  1 2 3 5 7 11 13    17    ) "
        expected = Right ([1,2,3,5,7,11,13,17]," ")

parserTestList :: Test
parserTestList = TestList [
    testParseAnyChar,
    testParseAnyCharFailEmpty,
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
    testParseIntNegative,
    testParsePair,
    testParseList
    ]
