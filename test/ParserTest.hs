module ParserTest (parserTestList) where
import Ast (GomExpr(..))
import Control.Applicative (Alternative(..))
import Test.HUnit
import Parser (parseAnyChar, parseChar, parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parseInt, parsePair, parseList, Parser, runParser,
    parserTokenChar, parseSymbol, parseNumber, parseBoolean, parseAtom,
    parseUntilAny, parseComment, parseGomExpr, parseCodeToGomExpr)

testParseChar :: Test
testParseChar = TestList
    [ TestCase $ assertEqual "parseChar invalid" expected1 result1
    , TestCase $ assertEqual "parseChar valid" expected2 result2
    ]
    where
        result1 = runParser (parseChar 'a') "1bcd"
        expected1 = Left "Expected 'a' but got '1'."

        result2 = runParser (parseChar 'a') "a"
        expected2 = Right ('a', "")

testParseCharFail :: Test
testParseCharFail = TestCase $ assertEqual "parseChar failure" expected result
    where
        result = runParser (parseChar 'a') "defg"
        expected = Left "Expected 'a' but got 'd'."

testParseCharEmpty :: Test
testParseCharEmpty = TestCase $ assertEqual "parseChar empty" expected result
    where
        result = runParser (parseChar 'a') ""
        expected = Left "Expected 'a' but got empty string."

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

testParseAndSecondFail :: Test
testParseAndSecondFail = TestCase $ assertEqual "parseAnd fail" expected result
    where
        result = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "ac"
        expected = Left "Expected 'b' but got 'c'."

testParseAndWith :: Test
testParseAndWith = TestCase $ assertEqual "parseAndWith valid" expected result
    where
        result = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Right ("ab", "cd")

testParseAndWithFail :: Test
testParseAndWithFail = TestCase $ assertEqual "parseAndWith fail" expected result
    where
        result = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "ac"
        expected = Left "Expected 'b' but got 'c'."

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

testParseManyEmpty :: Test
testParseManyEmpty = TestCase $ assertEqual "parseMany empty" expected result
    where
        result = runParser (parseMany (parseChar ' ')) ""
        expected = Right ("", "")

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

testParseIntFail :: Test
testParseIntFail = TestCase $ assertEqual "parseInt fail" expected result
    where
        result = runParser (parseInt :: Parser Int) "foobar"
        expected = Left "Expected one of '0123456789' but got 'f'."

testParsePair :: Test
testParsePair = TestCase $ assertEqual "parsePair valid" expected result
    where
        result = runParser (parsePair parseInt) "(123 456)foo bar"
        expected = Right ((123, 456), "foo bar")

testParsePairFail :: Test
testParsePairFail = TestCase $ assertEqual "parsePair fail" expected result
    where
        result = runParser (parsePair parseInt) "(123 456 foo bar"
        expected = Left "Expected ')' but got 'f'."

testParseList :: Test
testParseList = TestCase $ assertEqual "parseList valid" expected result
    where
        result = runParser (parseList parseInt) "(  1 2 3 5 7 11 13    17    ) "
        expected = Right ([1,2,3,5,7,11,13,17]," ")

testParseListFail :: Test
testParseListFail = TestCase $ assertEqual "parseList fail" expected result
    where
        result = runParser (parseList parseInt) "(  1 2 3 5 7 11 13    17    "
        expected = Left "Expected ')' but got empty string."

testParserTokenChar :: Test
testParserTokenChar = TestCase $ assertEqual "all characters valid" expected result
    where
        result = parserTokenChar
        expected = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*/<>=?"

testParseSymbol :: Test
testParseSymbol = TestCase $ assertEqual "parseSymbol valid" expected result
    where
        result = runParser parseSymbol "foobar"
        expected = Right (Symbol "foobar", "")

testParseNumber :: Test
testParseNumber = TestCase $ assertEqual "parseNumber valid" expected result
    where
        result = runParser parseNumber "42"
        expected = Right (Number 42, "")

testParseBoolean :: Test
testParseBoolean = TestCase $ assertEqual "parseBoolean valid" expected result
    where
        result = runParser parseBoolean "#t"
        expected = Right (Boolean True, "")

testParseAtom :: Test
testParseAtom = TestList
    [ TestCase $ assertEqual "parseAtom valid number" expected1 result1
    , TestCase $ assertEqual "parseAtom valid symbol" expected2 result2
    ]
    where
        result1 = runParser parseAtom "42"
        expected1 = Right (Number 42, "")

        result2 = runParser parseAtom "abc"
        expected2 = Right (Symbol "abc", "")

testParseUntilAny :: Test
testParseUntilAny = TestList
    [ TestCase $ assertEqual "parseUntilAny valid" expected1 result1
    , TestCase $ assertEqual "parseUntilAny valid" expected2 result2
    ]
    where
        result1 = runParser (parseUntilAny "abc") "def"
        expected1 = Left "Expected one of 'abc' but got empty string."

        result2 = runParser (parseUntilAny "abc") "defabc"
        expected2 = Right ("def", "abc")

testParseComment :: Test
testParseComment = TestCase $ assertEqual "parseComment valid" expected result
    where
        result = runParser parseComment "; this is a comment\n"
        expected = Right (" this is a comment", "\n")

testParseGomExpr :: Test
testParseGomExpr = TestCase $ assertEqual "parseGomExpr valid" expected result
    where
        result = runParser parseGomExpr "(+ 1 2)"
        expected = Right (List [Symbol "+", Number 1, Number 2], "")


testParseCodeToGomExpr :: Test
testParseCodeToGomExpr = TestCase $ assertEqual "parseCodeToGomExpr valid" expected result
    where
        result = runParser parseCodeToGomExpr "(+ 1 2)"
        expected = Right ([List [Symbol "+", Number 1, Number 2]], "")

testEmpty :: Test
testEmpty = TestCase $ assertEqual "empty" expected result
    where
        result = runParser (empty :: Parser Char) "abc"
        expected = Left "Empty parser"

parserTestList :: Test
parserTestList = TestList [
    testParseChar,
    testParseCharFail,
    testParseCharEmpty,
    testParseAnyChar,
    testParseAnyCharFailEmpty,
    testParseAnyCharFail,
    testParseOr,
    testParseOrFirstFail,
    testParseOrBothFail,
    testParseAnd,
    testParseAndFirstFail,
    testParseAndSecondFail,
    testParseAndWith,
    testParseAndWithFail,
    testParseManyOne,
    testParseManyTwo,
    testParseManyEmpty,
    testParseSomeValid,
    testParseSomeFail,
    testParseInt,
    testParseIntTwo,
    testParseIntNegative,
    testParseIntFail,
    testParsePair,
    testParsePairFail,
    testParseList,
    testParseListFail,
    testParserTokenChar,
    testParseSymbol,
    testParseNumber,
    testParseBoolean,
    testParseAtom,
    testParseComment,
    testParseUntilAny,
    testParseCodeToGomExpr,
    testEmpty,
    testParseGomExpr
    ]
