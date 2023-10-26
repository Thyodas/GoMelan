module ParserTest (parserTestList) where
import Ast (GomExpr(..), GomExprType(..))
import Control.Applicative (Alternative(..))
import Test.HUnit
import Parser (parseAnyChar, parseChar, parseOr, parseAnd, parseAndWith,
    parseMany, parseSome, parsePair, parseNumber, Parser, runParser,
    parserTokenChar, parseIdentifier, parseNumber, parseBoolean, parseString,
    parseStatement, parseReturnStatement, parseExpression, parseTermWithOperator,
    parseOperatorAnd, parseOperatorNot, parseOperatorNotEqual, parseOperatorEqual,
    parseOperatorModulo, parseOperatorInf, parseOperatorSup, parseOperatorInfEqual,
    parseOperatorSupEqual, parseOperatorDivide, parseOperatorMultiply, parseOperatorMinus,
    handleOtherCases, parseFactorWithOperator, parseTermWithoutOperator, parseType, parseSemicolumn,
    parseFunctionName, parseBlock, parseReturnType, parseModule, parseImportIdentifier, parseCustomType,
    parseList, parseTerm, parseTermWithoutOperator, parseSemicolumn, parseTypedIdentifier, parseParameter,
    parseAssignent, parseForLoopIter, parseCondition, parseIncludeList, parseFunctionCall,
    parseOperatorPlus, parseBinaryOperator,parseUntilAny, parseComment, parseGomExpr, parseBetween, parseCodeToGomExpr)

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
        expected = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

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

-- testParseBoolean :: Test
-- testParseBoolean = TestCase $ assertEqual "parseBoolean valid" expected result
--     where
--         result = runParser parseBoolean "#t"
--         expected = Right (Boolean True, "")

testParseBoolean :: Test
testParseBoolean = TestList
    [ TestCase $ assertEqual "parseBoolean valid" expected1 result1
    , TestCase $ assertEqual "parseBoolean not valid" expected2 result2
    ]
    where
        result1 = runParser parseBoolean "True"
        expected1 = Right (Boolean True, "")

        result2 = runParser parseBoolean "False"
        expected2 = Right (Boolean False, "")

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
    , TestCase $ assertEqual "parseUntilAny invalid" expected2 result2
    , TestCase $ assertEqual "parseUntilAny no match" expected3 result3
    ]
    where
        result1 = runParser (parseUntilAny " \n\t") "this is a test\n"
        expected1 = Right ("this", "is a test\n")

        result2 = runParser (parseUntilAny " \n\t") ""
        expected2 = Left "Expected one of ' \n\t' but got empty string."

        -- Test case with no match
        result3 = runParser (parseUntilAny "abc") "123456"
        expected3 = Left "Expected one of 'abc' but got empty string."


testParseComment :: Test
testParseComment = TestList
    [ TestCase $ assertEqual "parseComment valid" expected1 result1
    , TestCase $ assertEqual "parseComment invalid" expected2 result2
    ]
    where
        result1 = runParser parseComment "// this is a comment\n"
        expected1 = Right (" this is a comment", "")

        result2 = runParser parseComment "/ this is not a comment\n"
        expected2 = Left "Expected '/' but got ' '."


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
    ]
    where
        result1 = runParser parseReturnStatement "return 42;"
        expected1 = Right (Expression [Identifier "42"],"")


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

testParseOperatorEqual :: Test
testParseOperatorEqual = TestList
    [ TestCase $ assertEqual "parseOperatorEqual valid" expected1 result1
    ]
    where
        result1 = runParser parseOperatorEqual "=="
        expected1 = Right ("==", "")


testParseOperatorNotEqual :: Test
testParseOperatorNotEqual = TestList
    [ TestCase $ assertEqual "parseOperatorNotEqual valid" expected1 result1
    ]
    where
        result1 = runParser parseOperatorNotEqual "!="
        expected1 = Right ("!=", "")

testParseOperatorNot :: Test
testParseOperatorNot = TestList
    [ TestCase $ assertEqual "parseOperatorNot valid" expected1 result1
    ]
    where
        result1 = runParser parseOperatorNot "!"
        expected1 = Right ("!", "")


testParseOperatorAnd :: Test
testParseOperatorAnd = TestList
    [ TestCase $ assertEqual "parseOperatorAnd valid" expected1 result1
    ]
    where
        result1 = runParser parseOperatorAnd "&&"
        expected1 = Right ("&&", "")

testParseOperatorModulo :: Test
testParseOperatorModulo = TestList
    [ TestCase $ assertEqual "parseOperatorModulo valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorModulo invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorModulo "%"
        expected1 = Right ("%", "")

        result2 = runParser parseOperatorModulo "+"
        expected2 = Left "Expected '%' but got '+'."

testParseOperatorInf :: Test
testParseOperatorInf = TestList
    [ TestCase $ assertEqual "parseOperatorInf valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorInf invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorInf "<"
        expected1 = Right ("<", "")

        result2 = runParser parseOperatorInf ">"
        expected2 = Left "Expected '<' but got '>'."

testParseOperatorSup :: Test
testParseOperatorSup = TestList
    [ TestCase $ assertEqual "parseOperatorSup valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorSup invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorSup ">"
        expected1 = Right (">", "")

        result2 = runParser parseOperatorSup "<"
        expected2 = Left "Expected '>' but got '<'."

testParseOperatorInfEqual :: Test
testParseOperatorInfEqual = TestList
    [ TestCase $ assertEqual "parseOperatorInfEqual valid" expected1 result1
    ]
    where
        result1 = runParser parseOperatorInfEqual "<="
        expected1 = Right ("<=", "")


testParseOperatorSupEqual :: Test
testParseOperatorSupEqual = TestList
    [ TestCase $ assertEqual "parseOperatorSupEqual valid" expected1 result1
    ]
    where
        result1 = runParser parseOperatorSupEqual ">="
        expected1 = Right (">=", "")


testParseOperatorDivide :: Test
testParseOperatorDivide = TestList
    [ TestCase $ assertEqual "parseOperatorDivide valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorDivide invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorDivide "/"
        expected1 = Right ("/", "")

        result2 = runParser parseOperatorDivide "*"
        expected2 = Left "Expected '/' but got '*'."

testParseOperatorMultiply :: Test
testParseOperatorMultiply = TestList
    [ TestCase $ assertEqual "parseOperatorMultiply valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorMultiply invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorMultiply "*"
        expected1 = Right ("*", "")

        result2 = runParser parseOperatorMultiply "/"
        expected2 = Left "Expected '*' but got '/'."

testParseOperatorMinus :: Test
testParseOperatorMinus = TestList
    [ TestCase $ assertEqual "parseOperatorMinus valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorMinus invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorMinus "-"
        expected1 = Right ("-", "")

        result2 = runParser parseOperatorMinus "+"
        expected2 = Left "Expected '-' but got '+'."

testParseOperatorPlus :: Test
testParseOperatorPlus = TestList
    [ TestCase $ assertEqual "parseOperatorPlus valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorPlus invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorPlus "+"
        expected1 = Right ("+", "")

        result2 = runParser parseOperatorPlus "-"
        expected2 = Left "Expected '+' but got '-'."

testHandleOtherCases :: Test
testHandleOtherCases = TestList
    [ TestCase $ assertEqual "handleOtherCases valid" expected1 result1
    , TestCase $ assertEqual "handleOtherCases invalid" expected2 result2
    ]
    where
        result1 = runParser handleOtherCases "+"
        expected1 = Left "Expected a binary operator, but got '+'."

        result2 = runParser handleOtherCases ""
        expected2 = Left "Expected a binary operator, but got ''."

testParseSemicolumn :: Test
testParseSemicolumn = TestList
    [ TestCase $ assertEqual "parseSemicolumn valid" expected1 result1
    , TestCase $ assertEqual "parseSemicolumn invalid" expected2 result2
    ]
    where
        result1 = runParser (parseSemicolumn (parseChar 'a')) "a;"
        expected1 = Right ('a', "")

        result2 = runParser (parseSemicolumn (parseChar 'a')) "a"
        expected2 = Left "Expected ';' but got empty string."


testParseFunctionName :: Test
testParseFunctionName = TestList
    [ TestCase $ assertEqual "parseFunctionName valid" expected1 result1
    ]
    where
        result1 = runParser parseFunctionName "myFunction"
        expected1 = Right (Identifier "myFunction", "")


testParseModule :: Test
testParseModule = TestList
    [ TestCase $ assertEqual "parseModule valid" expected1 result1
    ]
    where
        result1 = runParser parseModule "module MyModule where"
        expected1 = Right (Identifier "module"," MyModule where")


testParseImportIdentifier :: Test
testParseImportIdentifier = TestList
    [ TestCase $ assertEqual "parseImportIdentifier valid" expected1 result1
    ]
    where
        result1 = runParser parseImportIdentifier "MyModule"
        expected1 = Right (Identifier "MyModule", "")


testParseCustomType :: Test
testParseCustomType = TestList
    [ TestCase $ assertEqual "parseCustomType valid" expected1 result1
    ]
    where
        result1 = runParser parseCustomType "Int"
        expected1 = Right (SingleType ("Int"), "")


testParseReturnType :: Test
testParseReturnType = TestList
    [ TestCase $ assertEqual "parseReturnType valid" expected1 result1
    ]
    where
        result1 = runParser parseReturnType "Int"
        expected1 = Right (Identifier "Int","")


testParseList :: Test
testParseList = TestList
    [ TestCase $ assertEqual "parseList valid" expected1 result1
    , TestCase $ assertEqual "parseList invalid" expected2 result2
    , TestCase $ assertEqual "parseList empty" expected3 result3
    , TestCase $ assertEqual "parseList single element" expected4 result4
    ]
    where
        result1 = runParser parseList "(1, 2, 3)"
        expected1 = Left "Expected ')' but got '1'."

        result2 = runParser parseList "(1, 2, 3"
        expected2 = Left "Expected ')' but got '1'."

        result3 = runParser parseList "()"
        expected3 = Right (List [],"")

        result4 = runParser parseList "(1)"
        expected4 = Left "Expected ')' but got '1'."

testParseFactorWithOperator :: Test
testParseFactorWithOperator = TestList
    [ TestCase $ assertEqual "parseFactorWithOperator valid" expected1 result1
    , TestCase $ assertEqual "parseFactorWithOperator invalid" expected2 result2
    , TestCase $ assertEqual "parseFactorWithOperator empty" expected3 result3
    , TestCase $ assertEqual "parseFactorWithOperator valid" expected4 result4
    ]
    where
        result1 = runParser parseFactorWithOperator "x + y"
        expected1 = Left "Expected a binary operator, but got ' '."

        result2 = runParser parseFactorWithOperator "x"
        expected2 = Left "Expected a binary operator, but got ''."

        result3 = runParser parseFactorWithOperator ""
        expected3 = Left "Expected 'F' but got empty string."

        result4 = runParser parseFactorWithOperator "1 + 2 * 3"
        expected4 = Left "Expected a binary operator, but got ' '."

testParseTerm :: Test
testParseTerm = TestList
    [ TestCase $ assertEqual "parseTerm valid" expected1 result1
    , TestCase $ assertEqual "parseTerm invalid" expected2 result2
    , TestCase $ assertEqual "parseTerm empty" expected3 result3
    , TestCase $ assertEqual "parseTerm single element" expected4 result4
    , TestCase $ assertEqual "parseTerm with close parenthesis" expected5 result5
    ]
    where
        result1 = runParser parseTerm "1 + 2"
        expected1 = Right (Term [Identifier "1",Operator "+",Identifier "2"],"")

        result2 = runParser parseTerm "1 +"
        expected2 = Right (Term [Identifier "1",Operator "+"],"")

        result3 = runParser parseTerm "()"
        expected3 = Left "Expected '(' but got ')'."

        result4 = runParser parseTerm "1"
        expected4 = Right (Term [Identifier "1"],"")

        result5 = runParser parseTerm "(1 + 2)"
        expected5 = Right (Term [Term [Identifier "1",Operator "+",Identifier "2"]],"")

testParseTermWithoutOperator :: Test
testParseTermWithoutOperator = TestList
    [ TestCase $ assertEqual "parseTermWithoutOperator valid" expected1 result1
    , TestCase $ assertEqual "parseTermWithoutOperator invalid" expected2 result2
    , TestCase $ assertEqual "parseTermWithoutOperator empty" expected3 result3
    , TestCase $ assertEqual "parseTermWithoutOperator single element" expected4 result4
    , TestCase $ assertEqual "parseTermWithoutOperator with close parenthesis" expected5 result5
    ]
    where
        result1 = runParser parseTermWithoutOperator "1 + 2"
        expected1 = Right (Term [Identifier "1",Operator "+",Identifier "2"],"")

        result2 = runParser parseTermWithoutOperator "1 +"
        expected2 = Right (Term [Identifier "1",Operator "+"],"")

        result3 = runParser parseTermWithoutOperator "()"
        expected3 = Left "Expected '(' but got ')'."

        result4 = runParser parseTermWithoutOperator "1"
        expected4 = Right (Term [Identifier "1"],"")

        result5 = runParser parseTermWithoutOperator "(1 + 2)"
        expected5 = Right (Term [Term [Identifier "1",Operator "+",Identifier "2"]],"")

testParseTermWithOperator :: Test
testParseTermWithOperator = TestList
    [ TestCase $ assertEqual "parseTermWithOperator valid" expected1 result1
    , TestCase $ assertEqual "parseTermWithOperator invalid" expected2 result2
    , TestCase $ assertEqual "parseTermWithOperator empty" expected3 result3
    , TestCase $ assertEqual "parseTermWithOperator valid" expected4 result4
    ]
    where
        result1 = runParser parseTermWithOperator "1 + 2"
        expected1 = Left "Expected a binary operator, but got ''."

        result2 = runParser parseTermWithOperator "1 +"
        expected2 = Left "Expected a binary operator, but got ''."

        result3 = runParser parseTermWithOperator "()"
        expected3 = Left "Expected '(' but got ')'."

        result4 = runParser parseTermWithOperator "(1 + 2) * 3"
        expected4 = Left "Expected a binary operator, but got ''."


testParseAssignent :: Test
testParseAssignent = TestList
    [ TestCase $ assertEqual "parseAssignent valid" expected1 result1
    , TestCase $ assertEqual "parseAssignent invalid" expected2 result2
    , TestCase $ assertEqual "parseAssignent empty" expected3 result3
    , TestCase $ assertEqual "parseAssignent with close parenthesis" expected4 result4
    ]
    where
        result1 = runParser parseAssignent "x = 1"
        expected1 = Right (Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Identifier "1"]},"")

        result2 = runParser parseAssignent "x ="
        expected2 = Left "Expected '(' but got empty string."

        result3 = runParser parseAssignent "()"
        expected3 = Left "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('."

        result4 = runParser parseAssignent "(x = 1) * 3"
        expected4 = Left "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('."

testParseForLoopIter :: Test
testParseForLoopIter = TestList
    [ "Test parseForLoopIter valid" ~:
        let input = "for (x = 0; x < 10; x++) { }"
            expected = Left "Expected ')' but got 'x'."
            result = runParser parseForLoopIter input
        in assertEqual "Should parse valid input" expected result
    , "Test parseForLoopIter invalid" ~:
        let input = "for (x = 0; x < 10; x++) { "
            expected = Left "Expected ')' but got 'x'."
            result = runParser parseForLoopIter input
        in assertEqual "Should handle invalid input" expected result
    , "Test parseForLoopIter without block" ~:
        let input = "for (x = 0; x < 10; x++)"
            expected = Left "Expected ')' but got 'x'."
            result = runParser parseForLoopIter input
        in assertEqual "Should handle input without block" expected result
    ]

testParseTypedIdentifier :: Test
testParseTypedIdentifier = TestList
    [ TestCase $ assertEqual "parseTypedIdentifier valid" expected1 result1
    , TestCase $ assertEqual "parseTypedIdentifier invalid" expected2 result2
    , TestCase $ assertEqual "parseTypedIdentifier empty" expected3 result3
    , TestCase $ assertEqual "parseTypedIdentifier with close parenthesis" expected4 result4
    ]
    where
        result1 = runParser parseTypedIdentifier "x : Int"
        expected1 = Right (TypedIdentifier {identifier = Identifier "x", identifierType = Type (SingleType "Int")},"")

        result2 = runParser parseTypedIdentifier "x :"
        expected2 = Left "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got empty string."

        result3 = runParser parseTypedIdentifier "()"
        expected3 = Left "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('."

        result4 = runParser parseTypedIdentifier "(x : Int) * 3"
        expected4 = Left "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('."


testParseCondition :: Test
testParseCondition = TestList
    [ "Test parseCondition with else block" ~:
        let input = "if (x > 0) { x = 1; } else { x = 2; }"
            expected = Right (Condition
                { gomIfCondition = Expression
                    [ Identifier "x"
                    , Operator ">"
                    , Identifier "0"
                    ]
                , gomIfTrue = Block
                    [ Assignment
                        { assignedIdentifier = Identifier "x"
                        , assignedExpression = Expression [Identifier "1"]
                        }
                    ]
                , gomIfFalse = Block
                    [ Assignment
                        { assignedIdentifier = Identifier "x"
                        , assignedExpression = Expression [Identifier "2"]
                        }
                    ]
                }, "")
            result = runParser parseCondition input
        in assertEqual "Should parse valid input with else block" expected result
    , "Test parseCondition without else block" ~:
        let input = "if (x > 0) { x = 1; }"
            expected = Right (Condition
                { gomIfCondition = Expression
                    [ Identifier "x"
                    , Operator ">"
                    , Identifier "0"
                    ]
                , gomIfTrue = Block
                    [ Assignment
                        { assignedIdentifier = Identifier "x"
                        , assignedExpression = Expression [Identifier "1"]
                        }
                    ]
                , gomIfFalse = Empty
                }, "")
            result = runParser parseCondition input
        in assertEqual "Should parse valid input without else block" expected result
    , "Test parseCondition with invalid input" ~:
        let input = "if (x > 0) { x = 1; "
            expected = Left "Expected '}' but got empty string."
            result = runParser parseCondition input
        in assertEqual "Should handle invalid input" expected result
    ]

testParseFunctionCall :: Test
testParseFunctionCall = TestList
    [ TestCase $ assertEqual "parseFunctionCall valid" expected1 result1
    , TestCase $ assertEqual "parseFunctionCall invalid" expected2 result2
    , TestCase $ assertEqual "parseFunctionCall empty" expected3 result3
    , TestCase $ assertEqual "parseFunctionCall with extra characters" expected4 result4
    ]
    where
        result1 = runParser parseFunctionCall "foo()"
        expected1 = Right (Statements [Identifier "foo"], "")

        result2 = runParser parseFunctionCall "foo"
        expected2 = Left "Expected '(' but got empty string."

        result3 = runParser parseFunctionCall "()"
        expected3 = Left "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('."

        result4 = runParser parseFunctionCall "foo() bar"
        expected4 = Right (Statements [Identifier "foo"], " bar")

parserTestList :: Test
parserTestList = TestList [
    testParseFunctionCall,
    testParseCondition,
    testParseTypedIdentifier,
    testParseForLoopIter,
    testParseAssignent,
    testParseTermWithOperator,
    testParseTermWithoutOperator,
    testParseTerm,
    testParseFactorWithOperator,
    testParseList,
    testParseReturnType,
    testParseCustomType,
    testParseImportIdentifier,
    testParseModule,
    testParseFunctionName,
    testParseSemicolumn,
    testHandleOtherCases,
    testparseNumber,
    testParseReturnStatement,
    testParseExpression,
    testParseOperatorPlus,
    testParseOperatorMinus,
    testParseOperatorMultiply,
    testParseOperatorDivide,
    testParseOperatorSupEqual,
    testParseOperatorInfEqual,
    testParseOperatorSup,
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
    testEmpty,
    testParseOperatorAnd,
    testParseOperatorNot,
    testParseOperatorNotEqual,
    testParseOperatorEqual,
    testParseOperatorModulo,
    testParseOperatorInf,
    -- testParseStatement,
    testParseString
    -- testParseReturnStatement,
    -- testParseExpression,
    -- testParseGomExpr
    ]
