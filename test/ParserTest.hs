module ParserTest (parserTestList) where
import Ast (GomExpr(..), GomExprType(..))
import Control.Applicative (Alternative(..))
import Test.HUnit
import Parser (parseAnyChar, parseChar, parseOr, parseAnd, parseAndWith, ErrorType(..), ParseError(..),
    parseMany, parseSome, parsePair, parseNumber, Parser, runParser, parseSep, ParseErrorStack(..),
    parserTokenChar, parseIdentifier, parseNumber, parseBoolean, parseString,
    parseStatement, parseReturnStatement, parseExpression, parseFunctionDeclaration,
    parseOperatorAnd, parseOperatorNot, parseOperatorNotEqual, parseOperatorEqual,
    parseOperatorModulo, parseOperatorInf, parseOperatorSup, parseOperatorInfEqual,
    parseOperatorSupEqual, parseOperatorDivide, parseOperatorMultiply, parseOperatorMinus,
    handleOtherCases, parseType, parseSemicolumn, parseFunctionName, parseBlock, parseReturnType,
    parseModule, parseImportIdentifier, parseCustomType, parseList, parseSemicolumn, parseTypedIdentifier,
    parseParameter, parseAssignent, parseForLoopIter, parseCondition, parseIncludeList, parseFunctionCall, parseParameterList,
    parseExpressionList, parseCodeToGomExpr, parseIncludeStatement, parseVariableDeclaration, parseLiteral,
    printErrorDetails, printErrors, printErrorLine, printLineWithError, runAndPrintParser,
    parseOperatorPlus, parseBinaryOperator,parseUntilAny, parseComment, parseGomExpr, parseBetween, parseCodeToGomExpr)

testShowErrorType :: Test
testShowErrorType = TestList [
        TestCase $ assertEqual "Show MissingClosing" "MissingClosing" (show MissingClosing),
        TestCase $ assertEqual "Show MissingOpening" "MissingOpening" (show MissingOpening),
        TestCase $ assertEqual "Show MissingIdentifier" "MissingIdentifier" (show MissingIdentifier),
        TestCase $ assertEqual "Show MissingOperator" "MissingOperator" (show MissingOperator),
        TestCase $ assertEqual "Show MissingExpression" "MissingExpression" (show MissingExpression),
        TestCase $ assertEqual "Show MissingChar" "MissingChar" (show MissingChar),
        TestCase $ assertEqual "Show MissingType" "MissingType" (show MissingType),
        TestCase $ assertEqual "Show MissingReturn" "MissingReturn" (show MissingReturn),
        TestCase $ assertEqual "Show MissingSemicolumn" "MissingSemicolumn" (show MissingSemicolumn),
        TestCase $ assertEqual "Show MissingFunctionName" "MissingFunctionName" (show MissingFunctionName),
        TestCase $ assertEqual "Show MissingFunctionArguments" "MissingFunctionArguments" (show MissingFunctionArguments),
        TestCase $ assertEqual "Show MissingFunctionBody" "MissingFunctionBody" (show MissingFunctionBody),
        TestCase $ assertEqual "Show MissingFunctionReturnType" "MissingFunctionReturnType" (show MissingFunctionReturnType),
        TestCase $ assertEqual "Show MissingFunctionCall" "MissingFunctionCall" (show MissingFunctionCall),
        TestCase $ assertEqual "Show MissingFunctionCallArguments" "MissingFunctionCallArguments" (show MissingFunctionCallArguments),
        TestCase $ assertEqual "Show MissingFunctionCallName" "MissingFunctionCallName" (show MissingFunctionCallName),
        TestCase $ assertEqual "Show EmptyParser" "EmptyParser" (show EmptyParser),
        TestCase $ assertEqual "Show InvalidBlock" "InvalidBlock" (show InvalidBlock)
    ]

testParseChar :: Test
testParseChar = TestList
    [ TestCase $ assertEqual "parseChar invalid" expected1 result1
    , TestCase $ assertEqual "parseChar valid" expected2 result2
    ]
    where
        result1 = runParser (parseChar 'a') "1bcd"
        expected1 = Left [ParseError MissingExpression "Expected 'a' but got '1'." "1bcd"]

        result2 = runParser (parseChar 'a') "a"
        expected2 = Right ('a', "")

testParseCharFail :: Test
testParseCharFail = TestCase $ assertEqual "parseChar failure" expected result
    where
        result = runParser (parseChar 'a') "defg"
        expected = Left [ParseError MissingExpression "Expected 'a' but got 'd'." "defg"]

testParseCharEmpty :: Test
testParseCharEmpty = TestCase $ assertEqual "parseChar empty" expected result
    where
        result = runParser (parseChar 'a') ""
        expected = Left [ParseError EmptyParser "Expected 'a' but got empty string." ""]

testParseAnyChar :: Test
testParseAnyChar = TestCase $ assertEqual "parseAnyChar" expected result
    where
        result = runParser (parseAnyChar "bca") "abcd"
        expected = Right ('a', "bcd")

testParseAnyCharFailEmpty :: Test
testParseAnyCharFailEmpty = TestCase $ assertEqual "parseAnyChar empty" expected result
    where
        result = runParser (parseAnyChar "bca") ""
        expected = Left [ParseError MissingChar "Expected any of 'bca' but got to the end." ""]

testParseAnyCharFail :: Test
testParseAnyCharFail = TestCase $ assertEqual "parseAnyChar failure" expected result
    where
        result = runParser (parseAnyChar "bca") "defg"
        expected = Left [ParseError MissingChar "Expected one of 'bca' but got 'd'." "defg"]

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
        expected = Left [ParseError MissingExpression "Expected 'b' but got 'd'." "defg"]


testParseAnd :: Test
testParseAnd = TestCase $ assertEqual "parseAnd valid" expected result
    where
        result = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Right (('a', 'b'), "cd")

testParseAndFirstFail :: Test
testParseAndFirstFail = TestCase $ assertEqual "parseAnd first failure" expected result
    where
        result = runParser (parseAnd (parseChar 'c') (parseChar 'b')) "bcd"
        expected = Left [ParseError MissingExpression "Expected 'c' but got 'b'." "bcd"]

testParseAndSecondFail :: Test
testParseAndSecondFail = TestCase $ assertEqual "parseAnd fail" expected result
    where
        result = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "ac"
        expected = Left [ParseError MissingExpression "Expected 'b' but got 'c'." "c"]

testParseAndWith :: Test
testParseAndWith = TestCase $ assertEqual "parseAndWith valid" expected result
    where
        result = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
        expected = Right ("ab", "cd")

testParseAndWithFail :: Test
testParseAndWithFail = TestCase $ assertEqual "parseAndWith fail" expected result
    where
        result = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "ac"
        expected = Left [ParseError MissingExpression "Expected 'b' but got 'c'." "c"]

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
        expected =  Left [ParseError MissingChar "Expected one of '0123456789' but got 'f'." "foobar42"]

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
        expected = Left [ParseError MissingChar "Expected one of '0123456789' but got 'f'." "foobar"]

testParsePair :: Test
testParsePair = TestCase $ assertEqual "parsePair valid" expected result
    where
        result = runParser (parsePair parseNumber) "(123 456)foo bar"
        expected = Right ((123, 456), "foo bar")

testParsePairFail :: Test
testParsePairFail = TestCase $ assertEqual "parsePair fail" expected result
    where
        result = runParser (parsePair parseNumber) "(123 456 foo bar"
        expected = Left [ParseError MissingExpression "Expected ')' but got 'f'." "foo bar"]

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
        expected2 = Left [ParseError MissingClosing "Expected any of  \n\tbut got to the end." ""]

        -- Test case with no match
        result3 = runParser (parseUntilAny "abc") "123456"
        expected3 = Left [ParseError MissingClosing "Expected any of abcbut got to the end." ""]

testParseComment :: Test
testParseComment = TestList
    [ TestCase $ assertEqual "parseComment valid" expected1 result1
    , TestCase $ assertEqual "parseComment invalid" expected2 result2
    ]
    where
        result1 = runParser parseComment "// this is a comment\n"
        expected1 = Right (" this is a comment", "")

        result2 = runParser parseComment "/ this is not a comment\n"
        expected2 = Left [ParseError MissingExpression "Expected '/' but got ' '." " this is not a comment\n"]

testEmpty :: Test
testEmpty = TestCase $ assertEqual "empty" expected result
    where
        result = runParser (empty :: Parser Char) "abc"
        expected = Left [ParseError EmptyParser "Empty parser" ""]

testParseBetween :: Test
testParseBetween = TestList
    [ TestCase $ assertEqual "parseBetween valid" expected1 result1
    , TestCase $ assertEqual "parseBetween valid" expected2 result2
    ]
    where
        result1 = runParser (parseBetween '(' ')' (parseChar 'a')) "(a)"
        expected1 = Right ('a', "")

        result2 = runParser (parseBetween '(' ')' (parseChar 'a')) "(b)"
        expected2 = Left [ParseError MissingExpression "Expected 'a' but got 'b'." "b)"]

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

testParseReturnStatement :: Test
testParseReturnStatement = TestList
    [ TestCase $ assertEqual "parseReturnStatement valid" expected1 result1
    ]
    where
        result1 = runParser parseReturnStatement "return 42;"
        expected1 = Right (Expression [Number 42],";")


testParseExpression :: Test
testParseExpression = TestList
    [ TestCase $ assertEqual "parseExpression with binary operator" expected1 result1
    , TestCase $ assertEqual "parseExpression with factor" expected2 result2
    , TestCase $ assertEqual "parseExpression with parentheses" expected3 result3
    ]
    where
        result1 = runParser parseExpression "1 + 2"
        expected1 = Right (Expression [Number 1,Operator "+",Number 2],"")

        result2 = runParser parseExpression "x"
        expected2 = Right (Expression [Identifier "x"],"")

        result3 = runParser parseExpression "(1 + 2)"
        expected3 = Right (Expression [Expression [Number 1,Operator "+",Number 2]],"")

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
        expected2 = Left [ParseError MissingExpression "Expected symbol '%'." "+", ParseError MissingExpression "Expected '%' but got '+'." "+"]

testParseOperatorInf :: Test
testParseOperatorInf = TestList
    [ TestCase $ assertEqual "parseOperatorInf valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorInf invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorInf "<"
        expected1 = Right ("<", "")

        result2 = runParser parseOperatorInf ">"
        expected2 = Left [ParseError MissingExpression "Expected symbol '<'." ">", ParseError MissingExpression "Expected '<' but got '>'." ">"]

testParseOperatorSup :: Test
testParseOperatorSup = TestList
    [ TestCase $ assertEqual "parseOperatorSup valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorSup invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorSup ">"
        expected1 = Right (">", "")

        result2 = runParser parseOperatorSup "<"
        expected2 = Left [ParseError MissingExpression "Expected symbol '>'." "<",ParseError MissingExpression "Expected '>' but got '<'." "<"]

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
        expected2 = Left [ParseError MissingExpression "Expected symbol '/'." "*",ParseError MissingExpression "Expected '/' but got '*'." "*"]

testParseOperatorMultiply :: Test
testParseOperatorMultiply = TestList
    [ TestCase $ assertEqual "parseOperatorMultiply valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorMultiply invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorMultiply "*"
        expected1 = Right ("*", "")

        result2 = runParser parseOperatorMultiply "/"
        expected2 = Left [ParseError MissingExpression "Expected symbol '*'." "/",ParseError MissingExpression "Expected '*' but got '/'." "/"]

testParseOperatorMinus :: Test
testParseOperatorMinus = TestList
    [ TestCase $ assertEqual "parseOperatorMinus valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorMinus invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorMinus "-"
        expected1 = Right ("-", "")

        result2 = runParser parseOperatorMinus "+"
        expected2 = Left [ParseError MissingExpression "Expected symbol '-'." "+",ParseError MissingExpression "Expected '-' but got '+'." "+"]

testParseOperatorPlus :: Test
testParseOperatorPlus = TestList
    [ TestCase $ assertEqual "parseOperatorPlus valid" expected1 result1
    , TestCase $ assertEqual "parseOperatorPlus invalid" expected2 result2
    ]
    where
        result1 = runParser parseOperatorPlus "+"
        expected1 = Right ("+", "")

        result2 = runParser parseOperatorPlus "-"
        expected2 = Left [ParseError MissingExpression "Expected symbol '+'." "-",ParseError MissingExpression "Expected '+' but got '-'." "-"]

testHandleOtherCases :: Test
testHandleOtherCases = TestList
    [ TestCase $ assertEqual "handleOtherCases valid" expected1 result1
    , TestCase $ assertEqual "handleOtherCases invalid" expected2 result2
    ]
    where
        result1 = runParser handleOtherCases "+"
        expected1 = Left [ParseError MissingOperator "Expected a binary operator, but got '+'." "+"]

        result2 = runParser handleOtherCases ""
        expected2 = Left [ParseError MissingOperator "Expected a binary operator, but got ''." ""]

testParseSemicolumn :: Test
testParseSemicolumn = TestList
    [ TestCase $ assertEqual "parseSemicolumn valid" expected1 result1
    , TestCase $ assertEqual "parseSemicolumn invalid" expected2 result2
    ]
    where
        result1 = runParser (parseSemicolumn (parseChar 'a')) "a;"
        expected1 = Right ('a', "")

        result2 = runParser (parseSemicolumn (parseChar 'a')) "a"
        expected2 = Left [ParseError EmptyParser "Expected ';' but got empty string." ""]


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
        result1 = runParser (parseList parseIdentifier) "(1, 2, 3)"
        expected1 = Right ([Identifier "1",Identifier "2",Identifier "3"],"")

        result2 = runParser (parseList parseIdentifier) "(1, 2, 3"
        expected2 = Left [ParseError EmptyParser "Expected ')' but got empty string." ""]

        result3 = runParser (parseList parseIdentifier) "()"
        expected3 = Right ([],"")

        result4 = runParser (parseList parseIdentifier) "(1)"
        expected4 = Right ([Identifier "1"],"")

testParseAssignent :: Test
testParseAssignent = TestList
    [ TestCase $ assertEqual "parseAssignent valid" expected1 result1
    , TestCase $ assertEqual "parseAssignent invalid" expected2 result2
    , TestCase $ assertEqual "parseAssignent empty" expected3 result3
    , TestCase $ assertEqual "parseAssignent with close parenthesis" expected4 result4
    ]
    where
        result1 = runParser parseAssignent "x = 1"
        expected1 = Right (Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 1]},"")

        result2 = runParser parseAssignent "x ="
        expected2 = Left [ParseError EmptyParser "Expected '(' but got empty string." ""]

        result3 = runParser parseAssignent "()"
        expected3 = Left [ParseError MissingChar "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('." "()"]

        result4 = runParser parseAssignent "(x = 1) * 3"
        expected4 = Left [ParseError MissingChar "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('." "(x = 1) * 3"]

testParseForLoopIter :: Test
testParseForLoopIter = TestList
    [ "Test parseForLoopIter valid" ~:
        let input = "for (x = 0; x < 10; x = x + 1) {}"
            expected = Left [ParseError InvalidBlock "Expected a block" "{}",ParseError MissingExpression "Expected symbol 'if'." "}",ParseError MissingExpression "Expected 'i' but got '}'." "}"]
            result = runParser parseForLoopIter input
        in assertEqual "Should parse valid input" expected result
    , "Test parseForLoopIter invalid" ~:
        let input = "for (x = 0; x < 10; x = x + 1) { "
            expected = Left [ParseError InvalidBlock "Expected a block" "{ ",ParseError MissingExpression "Expected symbol 'if'." "",ParseError EmptyParser "Expected 'i' but got empty string." ""]
            result = runParser parseForLoopIter input
        in assertEqual "Should handle invalid input" expected result
    , "Test parseForLoopIter variable assigment in block" ~:
        let input = "for (x = 0; x < 10; x = x + 1) { x : Int = 1; }"
            expected = Right (ForLoopIter {forLoopInitialization = Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 0]}, forLoopCondition = Expression [Identifier "x",Operator "<",Number 10], forLoopUpdate = Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Identifier "x",Operator "+",Number 1]}, forLoopIterBlock = Block [Assignment {assignedIdentifier = TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")}, assignedExpression = Expression [Number 1]}]},"")
            result = runParser parseForLoopIter input
        in assertEqual "Should handle invalid input" expected result
    , "Test parseForLoopIter pure empty" ~:
        let input = "for (x = 0; x < 10;) { x = 1; }"
            expected = Right (ForLoopIter {forLoopInitialization = Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 0]}, forLoopCondition = Expression [Identifier "x",Operator "<",Number 10], forLoopUpdate = Empty, forLoopIterBlock = Block [Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 1]}]},"")
            result = runParser parseForLoopIter input
        in assertEqual "Should handle invalid input" expected result
    , "Test parseForLoopIter expression in block" ~:
        let input = "for (; x < 10; x = x + 1) { x = 1; }"
            expected = Right (ForLoopIter {forLoopInitialization = Empty, forLoopCondition = Expression [Identifier "x",Operator "<",Number 10], forLoopUpdate = Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Identifier "x",Operator "+",Number 1]}, forLoopIterBlock = Block [Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 1]}]},"")
            result = runParser parseForLoopIter input
        in assertEqual "Should handle invalid input" expected result
    , "Test parseForLoopIter without block" ~:
        let input = "for (x = 0; x < 10) {}"
            expected = Left [ParseError MissingExpression "Expected ';' but got ')'." ") {}"]
            result = runParser parseForLoopIter input
        in assertEqual "Should handle input without block" expected result
    ]

testParseTypedIdentifier :: Test
testParseTypedIdentifier = TestList
    [ TestCase $ assertEqual "parseTypedIdentifier valid" expected1 result1
    , TestCase $ assertEqual "parseTypedIdentifier invalid" expected2 result2
    , TestCase $ assertEqual "parseTypedIdentifier empty" expected3 result3
    ]
    where
        result1 = runParser parseTypedIdentifier "x : Int"
        expected1 = Right (TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")},"")

        result2 = runParser parseTypedIdentifier "x :"
        expected2 = Left [ParseError MissingType "Expected a type." "",ParseError MissingChar "Expected any of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got to the end." ""]

        result3 = runParser parseTypedIdentifier "()"
        expected3 = Left [ParseError MissingChar "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('." "()"]


testParseCondition :: Test
testParseCondition = TestList
    [ "Test parseCondition with else block" ~:
        let input = "if (x > 0) { x = 1; } else { x = 2; }"
            expected = Right (Condition {gomIfCondition = Expression [Identifier "x",Operator ">",Number 0], gomIfTrue = Block [Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 1]}], gomIfFalse = Block [Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 2]}]},"")
            result = runParser parseCondition input
        in assertEqual "Should parse valid input with else block" expected result
    , "Test parseCondition without else block" ~:
        let input = "if (x > 0) { x = 1; }"
            expected = Right (Condition {gomIfCondition = Expression [Identifier "x",Operator ">",Number 0], gomIfTrue = Block [Assignment {assignedIdentifier = Identifier "x", assignedExpression = Expression [Number 1]}], gomIfFalse = Empty},"")
            result = runParser parseCondition input
        in assertEqual "Should parse valid input without else block" expected result
    , "Test parseCondition with invalid input" ~:
        let input = "if (x > 0) { x = 1; "
            expected = Left [ParseError InvalidBlock "Expected a block" "{ x = 1; ",ParseError MissingClosing "Expected '}' at the end of the block." "",ParseError EmptyParser "Expected '}' but got empty string." ""]
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
        expected1 = Right (FunctionCall {functionName = Identifier "foo", functionArguments = ParameterList []},"")

        result2 = runParser parseFunctionCall "foo"
        expected2 = Left [ParseError EmptyParser "Expected '(' but got empty string." ""]

        result3 = runParser parseFunctionCall "()"
        expected3 = Left [ParseError MissingChar "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('." "()"]

        result4 = runParser parseFunctionCall "foo() bar"
        expected4 = Right (FunctionCall {functionName = Identifier "foo", functionArguments = ParameterList []}," bar")


testParseParameter :: Test
testParseParameter = TestList
    [ TestCase $ assertEqual "parseParameter valid" expected1 result1
    , TestCase $ assertEqual "parseParameter invalid" expected2 result2
    , TestCase $ assertEqual "parseParameter empty" expected3 result3
    ]
    where
        result1 = runParser parseParameter "x : Int"
        expected1 = Right (TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")},"")

        result2 = runParser parseParameter "x :"
        expected2 = Left [ParseError MissingType "Expected a type." "",ParseError MissingChar "Expected any of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got to the end." ""]

        result3 = runParser parseParameter "()"
        expected3 = Left [ParseError MissingChar "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '('." "()"]

testParseParameterList :: Test
testParseParameterList = TestList
    [ TestCase $ assertEqual "parseParameterList valid" expected1 result1
    ]
    where
        result1 = runParser parseParameterList "x : Int, y : Int"
        expected1 = Right (ParameterList [TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")},TypedIdentifier {identifier = "y", identifierType = Type (SingleType "Int")}],"")

testParseVariableDeclaration :: Test
testParseVariableDeclaration = TestList
    [ "Test parseVariableDeclaration with valid input" ~:
        let input = "x : Int = 42;"
            expected = Right (Assignment {assignedIdentifier = TypedIdentifier {identifier = "x", identifierType = Type (SingleType "Int")}, assignedExpression = Expression [Number 42]},";")
            result = runParser parseVariableDeclaration input
        in assertEqual "Should parse valid input" expected result
    , "Test parseVariableDeclaration with no value" ~:
        let input = "x : Int = ;"
            expected = Left [ParseError MissingExpression "Expected '(' but got ';'." ";"]
            result = runParser parseVariableDeclaration input
        in assertEqual "Should handle invalid input" expected result
    ]

testParseFunctionDeclaration :: Test
testParseFunctionDeclaration = TestList
    [ TestCase $ assertEqual "testParseFunctionDeclaration valid two arguments" expected1 result1
    , TestCase $ assertEqual "testParseFunctionDeclaration valid no argument" expected2 result2
    , TestCase $ assertEqual "testParseFunctionDeclaration invalid missing perenthesis 1" expected3 result3
    , TestCase $ assertEqual "testParseFunctionDeclaration invalid missing braquette 2" expected4 result4
    , TestCase $ assertEqual "testParseFunctionDeclaration invalid return type" expected5 result5
    , TestCase $ assertEqual "testParseFunctionDeclaration invalid symbol" expected6 result6
    , TestCase $ assertEqual "testParseFunctionDeclaration missing arrow" expected7 result7
    ]
    where
        result1 = runParser parseFunctionDeclaration "fn multiply(a: Int, b: Int) -> Int { c: Int = a; }"
        expected1 = Right (Function {fnName = "multiply", fnArguments = ParameterList [TypedIdentifier {identifier = "a", identifierType = Type (SingleType "Int")},TypedIdentifier {identifier = "b", identifierType = Type (SingleType "Int")}], fnBody = Block [Assignment {assignedIdentifier = TypedIdentifier {identifier = "c", identifierType = Type (SingleType "Int")}, assignedExpression = Expression [Identifier "a"]}], fnReturnType = Type (SingleType "Int")},"")

        result2 = runParser parseFunctionDeclaration "fn multiply() -> Int { return 0; }"
        expected2 = Right (Function {fnName = "multiply", fnArguments = ParameterList [], fnBody = Block [Expression [Number 0]], fnReturnType = Type (SingleType "Int")},"")

        result3 = runParser parseFunctionDeclaration "fn multiply(a: Int, b: Int -> Int {}"
        expected3 = Left [ParseError MissingExpression "Expected ')' but got '-'." "-> Int {}"]

        result4 = runParser parseFunctionDeclaration "fn multiply(a: Int, b: Int) -> Int }"
        expected4 = Left [ParseError InvalidBlock "Expected a block" "}",ParseError MissingOpening "Expected '{' at the start of the block." "}",ParseError MissingExpression "Expected '{' but got '}'." "}"]

        result5 = runParser parseFunctionDeclaration "fn multiply() ->  {}"
        expected5 = Left [ParseError MissingFunctionReturnType "Expected a return type after '->'." "{}",ParseError MissingType "Expected a type." "{}",ParseError MissingChar "Expected one of 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_' but got '{'." "{}"]

        result6 = runParser parseFunctionDeclaration "lfi multiply(a: Int, b: Int) -> Int {}"
        expected6 = Left [ParseError MissingExpression "Expected symbol 'fn'." "lfi multiply(a: Int, b: Int) -> Int {}",ParseError MissingExpression "Expected 'f' but got 'l'." "lfi multiply(a: Int, b: Int) -> Int {}"]

        result7 = runParser parseFunctionDeclaration "fn multiply(a: Int, b: Int) Int {}"
        expected7 = Left [ParseError MissingFunctionReturnType "Expected '->' after function arguments." "Int {}",ParseError MissingExpression "Expected symbol '->'." "Int {}",ParseError MissingExpression "Expected '-' but got 'I'." "Int {}"]

testParseIncludeList :: Test
testParseIncludeList = TestList
    [ "Test parseIncludeList with valid input" ~:
        let input = "(Foo, Bar, Baz)"
            expected = Right (List [Identifier "Foo", Identifier "Bar", Identifier "Baz"], "")
            result = runParser parseIncludeList input
        in assertEqual "Should parse valid input" expected result
    ]

testParseExpressionList :: Test
testParseExpressionList = TestList
    [ "Test parseExpressionList valid" ~:
        let input = "(1, 2, 3)"
            expected = Right (List [Expression [Number 1],Expression [Number 2],Expression [Number 3]],"")
            result = runParser parseExpressionList input
        in assertEqual "Should parse valid input" expected result
    , "Test parseExpressionList invalid" ~:
        let input = "(1, 2, 3"
            expected = Left [ParseError EmptyParser "Expected ')' but got empty string." ""]
            result = runParser parseExpressionList input
        in assertEqual "Should handle invalid input" expected result
    , "Test parseExpressionList empty" ~:
        let input = "()"
            expected = Right (List [], "")
            result = runParser parseExpressionList input
        in assertEqual "Should handle empty input" expected result
    ]

testParseCodeToGomExpr :: Test
testParseCodeToGomExpr = TestList
    [ TestCase $ assertEqual "parseCodeToGomExpr valid" expected1 result1
    ]
    where
        result1 = runParser parseCodeToGomExpr "x = 1"
        expected1 = Left [ParseError MissingExpression "Expected symbol 'fn'." "x = 1",ParseError MissingExpression "Expected 'f' but got 'x'." "x = 1"]

testParseIncludeStatement :: Test
testParseIncludeStatement = TestList
    [ "Test parseIncludeStatement with import all" ~:
        let input = "include * from Bar"
            expected = Right (IncludeStatement (Identifier "*") (Identifier "Bar"), "")
            result = runParser parseIncludeStatement input
        in assertEqual "Should parse valid input" expected result
    , "Test parseIncludeStatement with include list" ~:
        let input = "include (Foo, Bar) from Baz"
            expected = Right (IncludeStatement {includeList = List [Identifier "Foo",Identifier "Bar"], fromModule = Identifier "Baz"},"")
            result = runParser parseIncludeStatement input
        in assertEqual "Should parse valid input" expected result
    , "Test parseIncludeStatement without identifier" ~:
        let input = "include  from Baz"
            expected = Left [ParseError MissingIdentifier "Expected an identifier or a list of identifiers" "Baz",ParseError MissingExpression "Expected symbol 'from'." "Baz",ParseError MissingExpression "Expected 'f' but got 'B'." "Baz"]
            result = runParser parseIncludeStatement input
        in assertEqual "Should parse valid input" expected result
    ]

testParseType :: Test
testParseType = TestList
    [ "Test parseType with single type" ~:
        let input = "Int"
            expected = Right (Type (SingleType "Int"), "")
            result = runParser parseType input
        in assertEqual "Should parse valid input" expected result
    , "Test parseType with list type" ~:
        let input = "[Int]"
            expected = Right (Type (TypeList [SingleType "Int"]), "")
            result = runParser parseType input
        in assertEqual "Should parse valid input" expected result
    , "Test parseType with custom type" ~:
        let input = "MyCustomType"
            expected = Right (Type (SingleType "MyCustomType"), "")
            result = runParser parseType input
        in assertEqual "Should parse valid input" expected result
    ]

testPrintErrorDetails :: Test
testPrintErrorDetails = TestList
    [ "Print MissingIdentifier error details" ~: do
        let errorType = MissingIdentifier
        let errorMessage = "Missing identifier error message"
        let remaining = "Remaining code after error"
        let parseError = ParseError errorType errorMessage remaining
        let lineNum = 5
        let colNum = 10
        let result = printErrorDetails "Some code" parseError lineNum colNum
        let expected = "Line 6, Column 11\n               ^\nMissingIdentifier: Missing identifier error message\n"
        assertEqual "Printing MissingIdentifier error details" expected result
    ]

testPrintErrors :: Test
testPrintErrors = TestList
    [ "Print single error details" ~: do
        let code = "let x = 10;"
        let errorType = MissingIdentifier
        let errorMessage = "Missing identifier error message"
        let remaining = ";"
        let parseError = [ParseError errorType errorMessage remaining]
        let result = printErrors code parseError
        let expected = "Line 1, Column 11\n 1 | let x = 10;\n               ^\nMissingIdentifier: Missing identifier error message\n"
        assertEqual "Printing single error details" expected result
    , "Print multiple error details with separation" ~: do
        let code = "let x = 10;"
        let errorType1 = MissingIdentifier
        let errorMessage1 = "Missing identifier error message"
        let remaining1 = ";"
        let errorType2 = MissingFunctionName
        let errorMessage2 = "Missing function name error message"
        let remaining2 = "x = 10;"
        let parseError = [ParseError errorType1 errorMessage1 remaining1, ParseError errorType2 errorMessage2 remaining2]
        let result = printErrors code parseError
        let expected = "Line 1, Column 11\n 1 | let x = 10;\n               ^\nMissingIdentifier: Missing identifier error message\n----------\nLine 1, Column 5\n 1 | let x = 10;\n         ^\nMissingFunctionName: Missing function name error message\n"
        assertEqual "Printing multiple error details with separation" expected result
    , "stack empty" ~: do
        let code = "let x = 10;"
        let parseError = []
        let result = printErrors code parseError
        let expected = ""
        assertEqual "stack empty" expected result
    ]

testPrintLineWithError :: Test
testPrintLineWithError = TestList
    [ "Print line with error details" ~: do
        let code = "let x = 10;"
        let errorLine = 1
        let errorColumn = 5
        let result = printLineWithError (lines code) errorLine errorColumn
        let expected = " 1 | let x = 10;\n          ^\n"
        assertEqual "Printing line with error details" expected result
    ]

parserTestList :: Test
parserTestList = TestList [
    testPrintLineWithError,
    testPrintErrors,
    testPrintErrorDetails,
    testShowErrorType,
    testParseFunctionDeclaration,
    testParseType,
    testParseVariableDeclaration,
    testParseIncludeStatement,
    testParseCodeToGomExpr,
    testParseExpressionList,
    testParseIncludeList,
    testParseParameterList,
    testParseParameter,
    testParseFunctionCall,
    testParseCondition,
    testParseTypedIdentifier,
    testParseForLoopIter,
    testParseAssignent,
    -- testParseTermWithOperator,
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
