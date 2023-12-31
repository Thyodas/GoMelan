{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Parser
-}

module Parser where
import Control.Applicative (Alternative(..))
import Ast (GomExpr(..), GomExprType(..))
import Text.Printf (printf)

data ErrorType = MissingClosing
    | MissingOpening
    | MissingIdentifier
    | MissingOperator
    | MissingExpression
    | MissingChar
    | MissingType
    | MissingReturn
    | MissingSemicolumn
    | MissingFunctionName
    | MissingFunctionArguments
    | MissingFunctionBody
    | MissingFunctionReturnType
    | MissingFunctionCall
    | MissingFunctionCallArguments
    | MissingFunctionCallName
    | EmptyParser
    | InvalidBlock
    deriving (Show, Eq)

type Remaining = String

type ErrorMsg = String

data ParseError = ParseError ErrorType ErrorMsg Remaining
    deriving (Show, Eq)

type ParseErrorStack = [ParseError]

-- | Take the furthest error
takeFurthestError :: ParseErrorStack -> ParseErrorStack -> ParseErrorStack
takeFurthestError [] [] = []
takeFurthestError [] err = err
takeFurthestError err [] = err
takeFurthestError fstErr sndErr =
    if length fstErrorRemaining < length sndErrorRemaining
        then fstErr
        else sndErr
        where
            (ParseError _ _ fstErrorRemaining) = last fstErr
            (ParseError _ _ sndErrorRemaining) = last sndErr

throwParseError :: ErrorType -> ErrorMsg -> Parser a
throwParseError errType msg =
    Parser $ \str -> Left [ParseError errType msg str]

newtype Parser a = Parser {
    runParser :: String -> Either ParseErrorStack (a, String)
}

instance Functor Parser where
    fmap fct parser = case parser of
        Parser p -> Parser $ \str -> case p str of
            Right (result, str') -> Right (fct result, str')
            Left err -> Left err

instance Applicative Parser where
    -- pure takes a value and returns a parser that always succeeds with that value
    pure x = Parser $ \str -> Right (x, str)

    -- <*> applies a function within a parser to a value within a parser
    (Parser p1) <*> (Parser p2) = Parser $ \str -> case p1 str of
        Right (fct, str') -> case p2 str' of
            Right (result, str'') -> Right (fct result, str'')
            Left err -> Left err
        Left err -> Left err

instance Alternative Parser where
    -- empty represents a parser that always fails
    empty = Parser $ \_ -> Left [ParseError EmptyParser "Empty parser" ""]

    -- <|> tries the first parser and if it fails, it tries the second parser
    (Parser p1) <|> (Parser p2) = parseOr (Parser p1) (Parser p2)

instance Monad Parser where
    -- pure already exists in Applicative, no need to define return

    -- (>>=) applies a parser to a value, and then applies a function to the result
    (Parser p) >>= f = Parser $ \str -> case p str of
        Right (x, str') -> runParser (f x) str'
        Left err -> Left err

-- Implement an operator similar to <?> for custom error messages
(<?>) :: Parser a -> (Remaining -> ParseError) -> Parser a
(Parser p) <?> parseError = Parser $ \str -> case p str of
  Left err -> Left (parseError str : err)
  other -> other

-- Replace the last error in the stack with a custom error message
(<?!>) :: Parser a -> (Remaining -> ParseError) -> Parser a
(Parser p) <?!> parseError = Parser $ \str -> case p str of
    Left _ -> Left [parseError str]
    other -> other

-- | Skip white space return string
parserWhitespaceChar :: String
parserWhitespaceChar = " \n\t"

-- | Parse tocken return string
parserTokenChar :: String
parserTokenChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

-- | Parse specific char passed in arg and return a parser
parseChar :: Char -> Parser Char
parseChar char = Parser $ \str -> case str of
    (x:str') | x == char -> Right (char, str')
             | otherwise -> Left [ParseError MissingExpression
                ("Expected '" ++ [char] ++ "' but got '" ++ [x] ++ "'.") str]
    [] -> Left [ParseError EmptyParser
        ("Expected '" ++ [char] ++ "' but got empty string.") ""]

-- Parse between two characters using a given parser
parseBetween :: Char -> Char -> Parser a -> Parser a
parseBetween open close parser = do
    _ <- parseChar open
    result <- parser
    _ <- parseChar close
    return result

-- | Parse Statement
parseStatement :: Parser GomExpr
parseStatement = parseAmongWhitespace (
    parseSemicolumn (
        parseVariableDeclaration
        <|> parseReturnStatement
        <|> parseAssignent
        <|> parseExpression)
    <|> parseForLoopIter
    <|> parseCondition)

-- | Parse a return statement
parseReturnStatement :: Parser GomExpr
parseReturnStatement = do
    _ <- parseSymbol "return"
    expression <- parseAmongWhitespace parseExpression
    return $ ReturnStatement expression

-- | Parse an expression
parseMultiple :: Parser [GomExpr]
parseMultiple = do
    bin <- parseBinaryOperator <?!> ParseError MissingOperator
        "Expected a binary operator."
    expr <- parseSubExpression <?!> ParseError MissingExpression
        "Expected an expression."
    return [bin, expr]

parseSubExpression :: Parser GomExpr
parseSubExpression = parseAmongWhitespace (
    parseFactor <|> parseBetween '(' ')' parseExpression)

parseExpression :: Parser GomExpr
parseExpression = Expression <$> parseExpression'
    where
        parseExpression' :: Parser [GomExpr]
        parseExpression' =
            (:) <$> parseSubExpression <*> (concat <$> parseMany parseMultiple)

-- | Parse list assigment on list creation
parseListAssignement :: Parser GomExpr
parseListAssignement =
    List <$> (parseBetween '[' ']' (parseSep ',' parseExpression))

-- | Parse list with [index] and return an Access
parseAccess :: Parser GomExpr
parseAccess = do
    var <- parseUntilAny "["
    case runParser (parseExpression) var of
        Right (expression, []) -> do
            index <- parseExpression
            _ <- parseChar ']'
            return $ Access { accessList = expression, accessIndex = index }
        Right _ -> throwParseError MissingExpression
               "Expected an expression before []"
        Left _ -> throwParseError MissingExpression "Expected an expression."

-- | Parse characters bewteen " and "
parseString :: Parser GomExpr
parseString = do
    str <- parseChar '"' *> parseUntilAny ['"']
    return $ stringToCharacterList str

stringToCharacterList :: String -> GomExpr
stringToCharacterList = List <$> map Character

parseTypeChar :: Parser GomExpr
parseTypeChar = do
    _ <- parseChar '\''
    char <- parseUntilAny "'"
    case char of
        [x] -> return $ Character x
        _ -> throwParseError MissingExpression
            "Expected a char between ''."

-- | Parse factor
parseFactor :: Parser GomExpr
parseFactor = parseBoolean <|> (FloatNumber <$> parseFloat)
    <|> (Number <$> parseNumber) <|> parseTypeChar <|> parseFunctionCall
    <|> parseAssignmentPlusPlus <|> parseAssignmentOperator <|> parseAssignent
    <|> parseAccess  <|> parseIdentifier <|> parseLiteral
    <|> parseListAssignement

-- | Handle other cases in parse binary operators
handleOtherCases :: Parser String
handleOtherCases = Parser $ \str -> Left [
    ParseError MissingOperator
        ("Expected a binary operator, but got '" ++ take 1 str ++ "'." )
        str]

-- | Parse binary operators
parseBinaryOperator :: Parser GomExpr
parseBinaryOperator = Operator <$>
                (parseOperatorPlus <|> parseOperatorMinus <|>
                parseOperatorMultiply <|> parseOperatorDivide <|>
                parseOperatorModulo <|> parseOperatorEqual <|>
                parseOperatorNotEqual <|> parseOperatorNot <|>
                parseOperatorAnd <|> parseOperatorInfEqual <|>
                parseOperatorSupEqual <|> parseOperatorInf <|>
                parseOperatorSup <|> parseOperatorOr)

-- | Parse a given string
parseSymbol :: String -> Parser String
parseSymbol str = parseSymbol' str <?> ParseError MissingExpression
                ("Expected symbol '" ++ str ++ "'.")
    where
        parseSymbol' [] = throwParseError EmptyParser
            "parseSymbol: Expected a symbol but got empty string."
        parseSymbol' [x] = (: []) <$> parseChar x
        parseSymbol' (x:xs) = do
            a <- (: []) <$> parseChar x
            b <- parseSymbol' xs
            return (a ++ b)

-- | Parse operator ADD '+'
parseOperatorPlus :: Parser String
parseOperatorPlus = parseSymbol "+"

-- | Parse operator SUB '-'
parseOperatorMinus :: Parser String
parseOperatorMinus = parseSymbol "-"

-- | Parse operator MUL ''
parseOperatorMultiply :: Parser String
parseOperatorMultiply = parseSymbol "*"

-- | Parse operator DIV '/'
parseOperatorDivide :: Parser String
parseOperatorDivide = parseSymbol "/"

-- | Parse operator MOD '>='
parseOperatorSupEqual :: Parser String
parseOperatorSupEqual = parseSymbol ">="

-- | Parse operator INFEQUAL '<='
parseOperatorInfEqual :: Parser String
parseOperatorInfEqual = parseSymbol "<="

-- | Parse operator SUP '>'
parseOperatorSup :: Parser String
parseOperatorSup = parseSymbol ">"

-- | Parse operator INF '<'
parseOperatorInf :: Parser String
parseOperatorInf = parseSymbol "<"

-- | Parse operator MOD '/'
parseOperatorModulo :: Parser String
parseOperatorModulo = parseSymbol "%"

-- | Parse operator EQUAL '=='
parseOperatorEqual :: Parser String
parseOperatorEqual = parseSymbol "=="

-- | Parse operator NOT EQUAL '!='
parseOperatorNotEqual :: Parser String
parseOperatorNotEqual = parseSymbol "!="

-- | Parse operator NOT '!'
parseOperatorNot :: Parser String
parseOperatorNot = parseSymbol "!"

-- | Parse operator AND '&&'
parseOperatorAnd :: Parser String
parseOperatorAnd = parseSymbol "&&"

-- | Parse operator OR '||'
parseOperatorOr :: Parser String
parseOperatorOr = parseSymbol "||"

parseTypeStringToCharList :: Parser GomExprType
parseTypeStringToCharList = do
    _ <- parseSymbol "String"
    return $ TypeList [SingleType "Char"]

-- | Parse all type and even custom type
parseType :: Parser GomExpr
parseType = Type <$> parseType' <?> ParseError MissingType "Expected a type."
    where
        parseType' :: Parser GomExprType
        parseType' = SingleType <$> (parseSymbol "Int"
                <|> parseSymbol "Char"
                <|> parseSymbol "Bool")
            <|> parseTypeStringToCharList
            <|> TypeList <$> (: []) <$> parseBetween '[' ']' parseType'
            <|> parseCustomType

-- | Parse a typed identifier
parseTypedIdentifier :: Parser GomExpr
parseTypedIdentifier = do
    name <- parseToken
    _ <- parseAmongWhitespace $ parseChar ':'
    idType <- parseAmongWhitespace $ parseType
    return $ TypedIdentifier {identifier=name, identifierType=idType}

-- | Parse a parameter
parseParameter :: Parser GomExpr
parseParameter = parseTypedIdentifier

-- | Apply parser on each element of a list separated by a char
-- | Example: parseSep ',' "a,b,c,d" -> [a, b, c, d]
-- | Note: you need to handle whitespaces yourself
parseSep :: Char -> Parser a -> Parser [a]
parseSep sep parser = parseMany (parseSep' sep parser)
    where
        parseSep' :: Char -> Parser a -> Parser a
        parseSep' sep' parser' = do
            result <- parser'
            _ <- parseChar sep' <|> pure ' '
            return result

-- -- | Apply parser on each element of a list separated by another parser
-- -- | Example: parseSepBy (parseSymbol ",") "a,b,c,d" -> [a, ",", b, ",", c, ",", d]
-- -- | Note: you need to handle whitespaces yourself
-- parseSepBy :: Parser a -> Parser a -> Parser [a]
-- -- parseSepBy sepParser parser = (:) <$> (parser <*> sepParser) <*> parseSepBy sepParser parser  <|> pure []
-- parseSepBy sepParser parser = (:) <$> parser <*> parseMany parseEach
--     where
--         parseEach :: Parser [a]
--         parseEach = (:) <$> sepParser <*> parser

-- | Parse list of parameter
parseParameterList :: Parser GomExpr
parseParameterList = ParameterList <$> parseSep ','
                    (parseAmongWhitespace parseParameter)

-- | Parse list of call function parameter
parseCallParameterList :: Parser GomExpr
parseCallParameterList = ParameterList <$> parseSep ','
                    (parseAmongWhitespace parseExpression)

-- | Parse specific char of a string passed in arg and return a parser
parseAnyChar :: String -> Parser Char
parseAnyChar toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right (x, str')
             | otherwise -> Left [ParseError MissingChar ("Expected one of '"
                ++ toFind
                ++ "' but got '" ++ [x] ++ "'.") str]
    [] -> Left [ParseError MissingChar ("Expected any of '" ++ toFind ++
                "' but got to the end.") ""]

-- | Parse a literal
parseLiteral :: Parser GomExpr
parseLiteral =  (FloatNumber <$> parseFloat) <|> (Number <$> parseNumber)
    <|> parseString <|> parseBoolean

-- | Takes two parser in arg, try to apply the first one if fail try the second and return a parser if one success
parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser $ \str -> case runParser parser1 str of
    Left fstErr -> case runParser parser2 str of
        Left sndErr -> Left (takeFurthestError fstErr sndErr)
        other -> other
    other -> other

-- | Takes two parser in arg, try the first one if success try the second and return a parser if both success
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 = (,) <$> parser1 <*> parser2

-- | Takes two parser and a function in arg, try to apply the parsers if success execute the function on the resulta nd return a parser
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func parser1 parser2 = func <$> parser1 <*> parser2

-- | Takes a parser in arg and try to apply it one or more times and return a list with all parsed element
parseMany :: Parser a -> Parser [a]
parseMany parser = (:) <$> parser <*> parseMany parser <|> pure []

parseEmptyListIfEnd :: Parser [a]
parseEmptyListIfEnd = Parser $ \str -> case str of
    [] -> Right ([], [])
    _ -> Left [ParseError MissingChar "Expected end of string." (str ++ "END")]

-- | Takes a parser in arg and try to apply it at least one time if success return a list of the parsed elements
parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> parseMany parser

parseSomeThrowIfNotEnd :: Parser a -> Parser [a]
parseSomeThrowIfNotEnd parser = (:) <$> parser <*> (parseSome parser
    <|> parseEmptyListIfEnd)

parseUFloat :: Parser Float
parseUFloat = do
    nb <- parseSome (parseAnyChar ['0'..'9'])
    dot <- parseChar '.'
    dec <- parseSome (parseAnyChar ['0'..'9'])
    return $ read (nb ++ [dot] ++ dec)

parseFloat :: Parser Float
parseFloat = negate <$> (parseChar '-' *> parseUFloat)
    <|> (parseChar '+' *> parseUFloat)
    <|> parseUFloat

-- | Parse unsigned integer and return it
parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

-- | Parse integer and return int
parseNumber :: Parser Int
parseNumber = negate <$> (parseChar '-' *> parseUInt)
    <|> (parseChar '+' *> parseUInt)
    <|> parseUInt

parsePair :: Parser a -> Parser (a, a)
parsePair parser = do
    _ <- parseChar '('
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    left <- parser
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    right <- parser
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    _ <- parseChar ')'
    return (left, right)

-- | Parse a token following parserTokenChar and return a string
parseToken :: Parser String
parseToken = parseSome (parseAnyChar parserTokenChar)

-- | Parse a symbol as string and return a GomExpr
parseIdentifier :: Parser GomExpr
parseIdentifier = Identifier <$> parseToken

-- | Parse an assignment variable with ++ or --
parseAssignmentPlusPlus :: Parser GomExpr
parseAssignmentPlusPlus = do
    id' <- parseTypedIdentifier <|> parseIdentifier
    op <- parseAmongWhitespace $ (parseSymbol "++" <|> parseSymbol "--")
    return $ Assignment {assignedIdentifier=id',
                            assignedExpression =
                                Expression
                                    [id', Operator (take 1 op), Number 1]}


-- | Parse an assignment variable with +=, -=, *=, /= or %=
parseAssignmentOperator :: Parser GomExpr
parseAssignmentOperator =  do
    id' <- parseTypedIdentifier <|> parseIdentifier
    op <- parseAmongWhitespace $ (parseSymbol "+=" <|> parseSymbol "-=" <|>
        parseSymbol "*=" <|> parseSymbol "/=" <|> parseSymbol "%=")
    expression <- parseAmongWhitespace $ parseExpression
    return $ Assignment {assignedIdentifier=id',
                            assignedExpression=
                                Expression
                                    [id', Operator (take 1 op), expression]}

-- | Parse variable / fonction assigment
parseAssignent :: Parser GomExpr
parseAssignent = do
    id' <- parseTypedIdentifier <|> parseIdentifier
    _ <- parseAmongWhitespace $ parseChar '='
    expression <- parseAmongWhitespace $ parseExpression
    return $ Assignment {assignedIdentifier=id',
                            assignedExpression=expression}

-- | Parse for loop
parseForLoopIter :: Parser GomExpr
parseForLoopIter = do
    _ <- parseSymbol "for"
    _ <- parseAmongWhitespace $ parseChar '('
    (initialization, condition, update) <- parseLoopParts
    _ <- parseAmongWhitespace $ parseChar ')'
    block <- parseAmongWhitespace parseBlock
    return $ ForLoopIter initialization condition update block

parseLoopParts :: Parser (GomExpr, GomExpr, GomExpr)
parseLoopParts = do
    initialization <- parseAmongWhitespace parseForLoopInitialization
    _ <- parseAmongWhitespace $ parseChar ';'
    condition <- parseAmongWhitespace parseForLoopCondition
    _ <- parseAmongWhitespace $ parseChar ';'
    update <- parseAmongWhitespace parseForLoopUpdate
    return (initialization, condition, update)


-- | Parse initialization part of a for loop
parseForLoopInitialization :: Parser GomExpr
parseForLoopInitialization = parseVariableDeclaration <|> parseAssignent <|>
    parseExpression <|> pure Empty

-- | Parse condition part of a for loop
parseForLoopCondition :: Parser GomExpr
parseForLoopCondition = parseExpression

-- | Parse a value assigment or nothing (empty)
parseForLoopUpdate :: Parser GomExpr
parseForLoopUpdate = parseAssignent <|> parseExpression <|> pure Empty

-- | Parse a boolean and return a GomExpr
parseBoolean :: Parser GomExpr
parseBoolean = do
    parsed <- parseSymbol "true" <|> parseSymbol "false"
    return (Boolean (parsed == "true"))

-- | parse until any of the given characters is found
parseUntilAny :: String -> Parser String
parseUntilAny toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right ("", str')
             | otherwise -> case runParser (parseUntilAny toFind) str' of
                Right (result, str'') -> Right (x:result, str'')
                Left err -> Left err
    [] -> Left [ParseError MissingClosing ("Expected any of " ++ toFind ++
                "but got to the end.") ""]

-- | parse until any of the given string is found
parseUntilSymbol :: String -> Parser String
parseUntilSymbol toFind = Parser $ \str -> case str of
    str'@(x:xs) | (take (length toFind) str') == toFind -> Right ("",
            drop (length toFind) str')
         | otherwise -> case runParser (parseUntilSymbol toFind) xs of
                Right (result, str'') -> Right (x:result, str'')
                Left err -> Left err
    [] -> Left [ParseError MissingClosing ("Expected " ++ toFind ++
                "but got to the end.") ""]

-- | parse comment after // to skip them
parseComment :: Parser GomExpr
parseComment = do
    _ <- (parseChar '/' *> parseChar '/' *> parseUntilAny "\n") <|> pure []
    _ <- (parseSymbol "/*" *> parseUntilSymbol "*/") <|> pure []
    return $ Empty

-- Parse an if statement
parseCondition :: Parser GomExpr
parseCondition = do
    _ <- parseAmongWhitespace (parseSymbol "if")
    _ <- parseAmongWhitespace $ parseChar '('
    condition <- parseExpression
    _ <- parseAmongWhitespace $ parseChar ')'
    thenBlock <- parseAmongWhitespace parseBlock
    maybeElseBlock <- parseAmongWhitespace (parseElseBlock <|> pure Empty)
    return $ Condition {gomIfCondition=condition,
        gomIfTrue=thenBlock, gomIfFalse=maybeElseBlock}

-- Parse an else block
parseElseBlock :: Parser GomExpr
parseElseBlock = do
    _ <- parseAmongWhitespace (parseSymbol "else")
    elseBlock <- parseAmongWhitespace parseBlock
    return elseBlock


parseIncludeList :: Parser GomExpr
parseIncludeList = List <$> parseList parseImportIdentifier

-- Parse an include statement
parseIncludeStatement :: Parser GomExpr
parseIncludeStatement = do
    _ <- parseAmongWhitespace (parseSymbol "include")
    include <- (parseAmongWhitespace parseImportIdentifier <|>
        parseAmongWhitespace parseIncludeList)
    _ <- parseAmongWhitespace (parseSymbol "from") <?>
        ParseError MissingIdentifier
        "Expected an identifier or a list of identifiers"
    moduleName <- parseAmongWhitespace parseModule
    return $ IncludeStatement {includeList=include, fromModule=moduleName}

parseModule :: Parser String
parseModule = parseChar '"' *> parseUntilAny ['"']

parseImportIdentifier :: Parser GomExpr
parseImportIdentifier = parseIdentifier <|> Identifier <$> parseSymbol "*"

-- parse a declare type by user
parseCustomType :: Parser GomExprType
parseCustomType = SingleType <$> parseToken

-- Parse the return identifier at the end of a fct
parseReturnType :: Parser GomExpr
parseReturnType = parseIdentifier

-- | parse everything between ( and ) and return a list of GomExpr
parseList :: Parser a -> Parser [a]
parseList parser = do
    _ <- parseChar '('
    list <- parseSep ',' (parseAmongWhitespace parser)
    _ <- parseChar ')'
    return list

-- | Parser pour une déclaration de variable
parseVariableDeclaration :: Parser GomExpr
parseVariableDeclaration = do
    variableName <- parseAmongWhitespace parseToken
    _ <- parseChar ':'
    variableType <- parseAmongWhitespace parseType
    _ <- parseChar '='
    variableValue <- parseAmongWhitespace parseExpression
    return $ Assignment {assignedIdentifier=TypedIdentifier {
        identifier=variableName, identifierType=variableType},
        assignedExpression=variableValue}

-- | parse everything between { and } and return a list of GomExpr
parseBlock :: Parser GomExpr
parseBlock = do
    _ <- parseChar '{' <?> ParseError MissingOpening
        "Expected '{' at the start of the block."
    result <- parseAmongWhitespace $ parseSome parseStatement
    _ <- parseChar '}' <?> ParseError MissingClosing
        "Expected '}' at the end of the block."
    return $ Block result

    <?> ParseError InvalidBlock "Expected a block"

-- | Parse function prototype
parseFunctionPrototype :: Parser GomExpr
parseFunctionPrototype =  do
    _ <- parseAmongWhitespace $ parseSymbol "fn"
    fctName <- parseAmongWhitespace parseToken
    arguments <- parseFunctionDeclarationArgument
    returnType <- parseFunctionDeclarationReturnType
    _ <- parseAmongWhitespace $ parseChar ';'
    return $ FunctionPrototype {fnProtoName=fctName,
        fnProtoArguments=arguments, fnProtoReturnType=returnType}

-- | Parse name of functions
parseFunctionName :: Parser GomExpr
parseFunctionName = parseIdentifier


-- | Parser pour une déclaration de fonction
parseFunctionDeclarationArgument :: Parser GomExpr
parseFunctionDeclarationArgument = parseAmongWhitespace $ ParameterList <$>
        parseList parseParameter

parseFunctionDeclarationReturnType :: Parser GomExpr
parseFunctionDeclarationReturnType = do
    _ <- parseAmongWhitespace $ parseSymbol "->" <?> ParseError
        MissingFunctionReturnType "Expected '->' after function arguments."
    parseAmongWhitespace $ parseType
        <?> ParseError MissingFunctionReturnType
        "Expected a return type after '->'."

parseFunctionDeclaration :: Parser GomExpr
parseFunctionDeclaration = do
    _ <- parseAmongWhitespace $ parseSymbol "fn"
    fctName <- parseAmongWhitespace parseToken
    arguments <- parseFunctionDeclarationArgument
    returnType <- parseFunctionDeclarationReturnType
    body <- parseAmongWhitespace $ parseBlock
    return $ Function {fnName=fctName,
        fnArguments=arguments, fnReturnType=returnType, fnBody=body}

-- | Parser pour un appel de fonction
parseFunctionCall :: Parser GomExpr
parseFunctionCall = do
    fctName <- parseIdentifier
    arguments <- ParameterList <$> parseList parseExpression
    return $ FunctionCall {functionName=fctName,
        functionArguments=arguments}

parseAmongWhitespace :: Parser a -> Parser a
parseAmongWhitespace parser = do
    _ <- parseComment
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    result <- parser
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    _ <- parseComment
    return result

parseSemicolumn :: Parser a -> Parser a
parseSemicolumn parser = do
    result <- parser
    _ <- parseChar ';'
    return result

-- | Pour gérer une liste d'expressions
parseExpressionList :: Parser GomExpr
parseExpressionList = List <$> parseList parseExpression

-- | Ensuite, vous pouvez combiner tous ces parsers pour gérer la structure de votre langage
parseGomExpr :: Parser GomExpr
parseGomExpr = parseIncludeStatement <|>
    parseSemicolumn (parseVariableDeclaration) <|> parseFunctionPrototype
    <|> parseFunctionDeclaration

-- | Parse code to return GomExpr
parseCodeToGomExpr :: Parser [GomExpr]
parseCodeToGomExpr = parseSomeThrowIfNotEnd parseGomExpr

printErrorDetails :: String -> ParseError -> Int -> Int -> String
printErrorDetails code (ParseError errType msg _) lineNum colNum =
    printf "Line %d, Column %d\n" (lineNum + 1) (colNum + 1) ++
    printLineWithError (lines code) lineNum colNum ++
    printf "%s: %s\n" (show errType) msg

printErrors :: String -> ParseErrorStack -> String
printErrors _ [] = ""
printErrors code (err@(ParseError _ _ remaining) : rest) =
    let lenRemaining = length remaining
        consumed = length code - lenRemaining + 1
        consumedLines = lines (take consumed code)
        lineNum = length consumedLines - 1
        colNum = length (last consumedLines) - 1
    in printErrorDetails code err lineNum colNum ++ if null rest
    then "" else printf (replicate 10 '-') ++ "\n" ++ printErrors code rest

printErrorLine :: [String] -> Int -> Int -> String
printErrorLine codeLines lenBiggestLineNum lineNum
    | lineNum < 0 = ""
    | lineNum >= length codeLines = ""
    | otherwise = printf " %*d | %s\n" lenBiggestLineNum (lineNum + 1)
        (codeLines !! lineNum)

printLineWithError :: [String] -> Int -> Int -> String
printLineWithError codeLines lineNum col =
    printErrorLine codeLines lenBiggestLineNum (lineNum - 1) ++
    printErrorLine codeLines lenBiggestLineNum lineNum ++
    replicate (lenBiggestLineNum + length "  | " + col) ' ' ++ "^\n"
        where
            lenBiggestLineNum = length (show (lineNum + 1))


-- Define a function to run the parser and handle errors with line and column information
runAndPrintParser :: Show a => Parser a -> String -> IO ()
runAndPrintParser parser code =
    case runParser parser code of
        Right (result, _) -> print result
        Left errors ->
            putStrLn "Parser Errors:" >>
            putStr (printErrors code (reverse errors))
