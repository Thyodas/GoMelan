module ParserTest (parserTestList) where
import Ast (GomExpr(..))
import Control.Applicative (Alternative(..))
import Test.HUnit
import Parser (parseAnyChar, parseChar, parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parsePair, parseNumber, Parser, runParser,
    parserTokenChar, parseIdentifier, parseNumber, parseBoolean, parseString,
    parseStatement, parseReturnStatement, parseExpression, parseTermWithOperator,
    parseUntilAny, parseComment, parseGomExpr, parseBetween, parseCodeToGomExpr)

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

testparseNumber :: Test
testparseNumber = TestCase $ assertEqual "parseNumber valid" expected result
    where
        result = runParser (parseNumber :: Parser Int) "42foobar"
        expected = Right (42, "foobar")

testParseIntTwo :: Test
testParseIntTwo = TestCase $ assertEqual "parseNumber valid two" expected result
    where
        result = runParser (parseNumber :: Parser Int) "+100"
        expected = Right (100, "")

testParseIntNegative :: Test
testParseIntNegative = TestCase $ assertEqual "parseNumber negative" expected result
    where
        result = runParser (parseNumber :: Parser Int) "-42foobar"
        expected = Right (-42, "foobar")

testParseIntFail :: Test
testParseIntFail = TestCase $ assertEqual "parseNumber fail" expected result
    where
        result = runParser (parseNumber :: Parser Int) "foobar"
        expected = Left "Expected one of '0123456789' but got 'f'."

testParsePair :: Test
testParsePair = TestCase $ assertEqual "parsePair valid" expected result
    where
        result = runParser (parsePair parseNumber) "(123 456)foo bar"
        expected = Right ((123, 456), "foo bar")

testParsePairFail :: Test
testParsePairFail = TestCase $ assertEqual "parsePair fail" expected result
    where
        result = runParser (parsePair parseNumber) "(123 456 foo bar"
        expected = Left "Expected ')' but got 'f'."

-- testParseList :: Test
-- testParseList = TestCase $ assertEqual "parseList valid" expected result
--     where
--         result = runParser (parseList parseNumber) "(  1 2 3 5 7 11 13    17    ) "
--         expected = Right ([1,2,3,5,7,11,13,17]," ")

-- testParseListFail :: Test
-- testParseListFail = TestCase $ assertEqual "parseList fail" expected result
--     where
--         result = runParser (parseList parseNumber) "(  1 2 3 5 7 11 13    17    "
--         expected = Left "Expected ')' but got empty string."

testParserTokenChar :: Test
testParserTokenChar = TestCase $ assertEqual "all characters valid" expected result
    where
        result = parserTokenChar
        expected = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+-*/<>=?"

testparseIdentifier :: Test
testparseIdentifier = TestCase $ assertEqual "parseIdentifier valid" expected result
    where
        result = runParser parseIdentifier "foobar"
        expected = Right (Identifier "foobar", "")

-- testParseNumber :: Test
-- testParseNumber = TestCase $ assertEqual "parseNumber valid" expected result
--     where
--         result = runParser parseNumber "42"
--         expected = Right (Number 42, "")

testParseBoolean :: Test
testParseBoolean = TestCase $ assertEqual "parseBoolean valid" expected result
    where
        result = runParser parseBoolean "#t"
        expected = Right (Boolean True, "")

-- testParseAtom :: Test
-- testParseAtom = TestList
--     [ TestCase $ assertEqual "parseAtom valid number" expected1 result1
--     , TestCase $ assertEqual "parseAtom valid symbol" expected2 result2
--     ]
--     where
--         result1 = runParser parseAtom "42"
--         expected1 = Right (Number 42, "")

--         result2 = runParser parseAtom "abc"
--         expected2 = Right (Symbol "abc", "")

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
        expected = Right (List [Identifier "+", Number 1, Number 2], "")


testParseCodeToGomExpr :: Test
testParseCodeToGomExpr = TestCase $ assertEqual "parseCodeToGomExpr valid" expected result
    where
        result = runParser parseCodeToGomExpr "(+ 1 2)"
        expected = Right ([List [Identifier "+", Number 1, Number 2]], "")

testEmpty :: Test
testEmpty = TestCase $ assertEqual "empty" expected result
    where
        result = runParser (empty :: Parser Char) "abc"
        expected = Left "Empty parser"

testParseBetween :: Test
testParseBetween = TestList
    [ TestCase $ assertEqual "parseBetween valid" expected1 result1
    , TestCase $ assertEqual "parseBetween valid" expected2 result2
    ]
    where
        result1 = runParser (parseBetween '(' ')' (parseChar 'a')) "(a)"
        expected1 = Right ('a', "")

        result2 = runParser (parseBetween '(' ')' (parseChar 'a')) "(b)"
        expected2 = Left "Expected 'a' but got 'b'."

testParseString :: Test
testParseString = TestList
    [ TestCase $ assertEqual "parseString valid" expected1 result1
    , TestCase $ assertEqual "parseString valid" expected2 result2
    ]
    where
        result1 = runParser parseString "\"hello world\""
        expected1 = Right (GomString "hello world", "")

        result2 = runParser parseString "\"hello world\";"
        expected2 = Right (GomString "hello world", ";")

-- testParseStatement :: Test
-- testParseStatement = TestList
--     [ TestCase $ assertEqual "parseStatement variable declaration" expected1 result1
--     , TestCase $ assertEqual "parseStatement return statement" expected2 result2
--     , TestCase $ assertEqual "parseStatement assignment" expected3 result3
--     , TestCase $ assertEqual "parseStatement for loop" expected4 result4
--     , TestCase $ assertEqual "parseStatement condition" expected5 result5
--     ]
--     where
--         result1 = runParser parseStatement "let x = 42;"
--         expected1 = Right (VariableDeclaration "x" (Number 42), "")

--         result2 = runParser parseStatement "return 42;"
--         expected2 = Right (Expression (Number 42), "")

--         result3 = runParser parseStatement "x = 42;"
--         expected3 = Right (Assignment "x" (Number 42), "")

--         result4 = runParser parseStatement "for i in 1..10 do\n  print(i)\nend;"
--         expected4 = Right (ForLoopIter "i" (Number 1) (Number 10) (List [Identifier "print", Identifier "i"]), "")

--         result5 = runParser parseStatement "if x == 42 then\n  print(\"x is 42\")\nelse\n  print(\"x is not 42\")\nend;"
--         expected5 = Right (Condition (Operator (Identifier "==") (Identifier "x") (Number 42)) (List [Identifier "print", GomString "x is 42"]) (List [Identifier "print", GomString "x is not 42"]), "")

testParseReturnStatement :: Test
testParseReturnStatement = TestList
    [ TestCase $ assertEqual "parseReturnStatement valid" expected1 result1
    , TestCase $ assertEqual "parseReturnStatement invalid" expected2 result2
    ]
    where
        result1 = runParser parseReturnStatement "return 42;"
        expected1 = Right (Expression [Identifier "42"],"")

        result2 = runParser parseReturnStatement "return;"
        expected2 = Left "Expected an expression after 'return', but got empty string."

testParseExpression :: Test
testParseExpression = TestList
    [ TestCase $ assertEqual "parseExpression with binary operator" expected1 result1
    , TestCase $ assertEqual "parseExpression with factor" expected2 result2
    , TestCase $ assertEqual "parseExpression with parentheses" expected3 result3
    ]
    where
        result1 = runParser parseExpression "1 + 2"
        expected1 = Right (Expression [Identifier "1",Operator "+",Identifier "2"],"")

        result2 = runParser parseExpression "x"
        expected2 = Right (Expression [Identifier "x"],"")

        result3 = runParser parseExpression "(1 + 2)"
        expected3 = Right (Expression [Expression [Identifier "1",Operator "+",Identifier "2"]],"")


parserTestList :: Test
parserTestList = TestList [
    testParseChar,
    testParseBetween,
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
    -- testParseInt,
    testParseIntTwo,
    testParseIntNegative,
    testParseIntFail,
    testParsePair,
    testParsePairFail,
    -- testParseList,
    -- testParseListFail,
    testParserTokenChar,
    testparseIdentifier,
    -- testParseNumber,
    testParseBoolean,
    -- testParseAtom,
    testParseComment,
    testParseUntilAny,
    testParseCodeToGomExpr,
    testEmpty,
    testParseString,
    -- testParseStatement,
    -- testParseReturnStatement,
    -- testParseExpression,
    testParseGomExpr
    ]
