{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Parser
-}

-- module Parser (
--     ErrorMsg, parseCodeToGomExpr, Parser(..), parseChar, parseAnyChar, parseOr,
--     parseAnd, parseAndWith, parseMany, parseSome, parseInt, parsePair, parseList
--     ,parserTokenChar, parseIdentifier, parseNumber, parseBoolean, parseAtom,
--     parseUntilAny, parseComment, parseGomExpr, parseBlock
-- ) where
module Parser where
import Control.Applicative (Alternative(..))
import Ast (GomExpr(..), GomExprType(..))

type ErrorMsg = String

newtype Parser a = Parser {
    runParser :: String -> Either ErrorMsg (a, String)
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
    empty = Parser $ \_ -> Left "Empty parser"

    -- <|> tries the first parser and if it fails, it tries the second parser
    (Parser p1) <|> (Parser p2) = parseOr (Parser p1) (Parser p2)

instance Monad Parser where
    -- pure already exists in Applicative, no need to define return

    -- (>>=) applies a parser to a value, and then applies a function to the result
    (Parser p) >>= f = Parser $ \str -> case p str of
        Right (x, str') -> runParser (f x) str'
        Left err -> Left err

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
             | otherwise -> Left ("Expected '" ++ [char]
                ++ "' but got '" ++ [x] ++ "'.")
    [] -> Left ("Expected '" ++ [char] ++ "' but got empty string.")

-- Parse between two characters using a given parser
parseBetween :: Char -> Char -> Parser a -> Parser a
parseBetween open close parser = do
    _ <- parseChar open
    result <- parser
    _ <- parseChar close
    return result

-- | Parse characters bewteen " and "
parseString :: Parser GomExpr
parseString = GomString <$> (parseChar '"' *> parseUntilAny ['"'])

parseStatement :: Parser GomExpr
parseStatement = parseAmongWhitespace (
    parseSemicolumn (
        parseVariableDeclaration
        <|> parseReturnStatement
        <|> parseAssignent)
    <|> parseForLoopIter
    <|> parseCondition)

-- | Parse a return statement
parseReturnStatement :: Parser GomExpr
parseReturnStatement = do
    _ <- parseSymbol "return"
    expression <- parseExpression
    _ <- parseChar ';'
    return expression

-- | Parse an expression
parseExpression :: Parser GomExpr
parseExpression = Expression <$> (parseSome $ parseAmongWhitespace $ parseBinaryOperator <|> parseFactor <|> parseBetween '(' ')' parseExpression)

-- | Parse a term with a binary operator and another expression
-- parseTermWithOperator :: Parser GomExpr
-- parseTermWithOperator = do
--     term <- parseTerm
--     operator <- parseBinaryOperator
--     expression <- parseExpression
--     return $ case operator of
--         Identifier "+" -> Statements [(Identifier "+"), term, expression]
--         Identifier "-" -> Statements [(Identifier "-"), term, expression]
--         Identifier "*" -> Statements [(Identifier "*"), term, expression]
--         Identifier "/" -> Statements [(Identifier "/"), term, expression]
--         Identifier "%" -> Statements [(Identifier "%"), term, expression]
--         _          -> expression

-- | Parse a term without a binary operator
parseTermWithoutOperator :: Parser GomExpr
parseTermWithoutOperator = parseTerm

-- | Parse a term
parseTerm :: Parser GomExpr
parseTerm = Term <$> (parseSome $ parseAmongWhitespace $ parseBinaryOperator <|> parseFactor <|> parseBetween '(' ')' parseTerm)

-- | Parse a factor with a binary operator and another term
parseFactorWithOperator :: Parser GomExpr
parseFactorWithOperator = do
    factor <- parseFactor
    operator <- parseBinaryOperator
    term <- parseTerm
    return $ case operator of
        id@(Identifier _) -> Statements [id, factor, term]
        _          -> term

parseFactor :: Parser GomExpr
parseFactor = parseIdentifier <|> parseLiteral

-- | Handle other cases in parse binary operators
handleOtherCases :: Parser String
handleOtherCases = Parser $ \str -> Left ("Expected a binary operator, but got '" ++ take 1 str ++ "'.")

-- | Parse binary operators
parseBinaryOperator :: Parser GomExpr
parseBinaryOperator = Operator <$> (parseOperatorPlus <|>
                parseOperatorMinus <|>
                parseOperatorMultiply <|>
                parseOperatorDivide <|>
                parseOperatorModulo <|>
                parseOperatorEqual <|>
                parseOperatorNotEqual <|>
                parseOperatorNot <|>
                parseOperatorAnd <|>
                parseOperatorInfEqual <|>
                parseOperatorSupEqual <|>
                parseOperatorInf <|>
                parseOperatorSup <|>
                handleOtherCases)

-- | Parse a given string
parseSymbol :: String -> Parser String
parseSymbol [x] = (: []) <$> parseChar x
parseSymbol (x:xs) = do
    a <- (: []) <$> parseChar x
    b <- parseSymbol xs
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

-- | Parse all type and even custom type
parseType :: Parser GomExpr
parseType = Type <$> parseType'
    where
        parseType' :: Parser GomExprType
        parseType' = SingleType <$> (parseSymbol "Int"
                <|> parseSymbol "String"
                <|> parseSymbol "Bool")
            <|> TypeList <$> (: []) <$> parseBetween '[' ']' parseType'
            <|> parseCustomType

-- | Parse a typed identifier
parseTypedIdentifier :: Parser GomExpr
parseTypedIdentifier = do
    id <- parseIdentifier
    _ <- parseAmongWhitespace $ parseChar ':'
    idType <- parseAmongWhitespace $ parseType
    return $ TypedIdentifier {identifier=id, identifierType=idType}

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
        parseSep' sep parser = do
            result <- parser
            _ <- parseChar sep <|> pure ' '
            return result


-- | Parse list of parameter
parseParameterList :: Parser GomExpr
parseParameterList = ParameterList <$> parseSep ',' (parseAmongWhitespace parseParameter)

-- | Parse specific char of a string passed in arg and return a parser
parseAnyChar :: String -> Parser Char
parseAnyChar toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right (x, str')
             | otherwise -> Left ("Expected one of '" ++ toFind
                ++ "' but got '" ++ [x] ++ "'.")
    [] -> Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

-- | Parse a literal
parseLiteral :: Parser GomExpr
parseLiteral = (Number <$> parseNumber) <|> parseString <|> parseBoolean

-- | Takes two parser in arg, try to apply the first one if fail try the second and return a parser if one success
parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser $ \str -> case runParser parser1 str of
    Left _ -> runParser parser2 str
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

-- | Takes a parser in arg and try to apply it at least one time if success return a list of the parsed elements
parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> parseMany parser

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

-- | Takes a list in arg as a string, apply a parser on each element separated by whitesapce
parseContent :: Char -> Char -> Parser a -> Parser [a]
parseContent open close parser = do
    _ <- parseChar open
    _ <- parseMany (parseAnyChar " \n\t")
    result <- parseMany (parser <* parseMany
        (parseAnyChar parserWhitespaceChar))
    _ <- parseChar close
    return result

-- | Parse a token following parserTokenChar and return a string
parseToken :: Parser String
parseToken = parseSome (parseAnyChar parserTokenChar)

-- | Parse a symbol as string and return a GomExpr
parseIdentifier :: Parser GomExpr
parseIdentifier = Identifier <$> parseToken

-- | Parse variable / fonction assigment
parseAssignent :: Parser GomExpr
parseAssignent = do
    identifier <- parseTypedIdentifier <|> parseIdentifier
    _ <- parseAmongWhitespace $ parseChar '='
    expression <- parseAmongWhitespace $ parseExpression
    return $ Assignment {assignedIdentifier=identifier, assignedExpression=expression}

-- | Parse for loop
parseForLoopIter :: Parser GomExpr
parseForLoopIter = do
    symbol <- parseSymbol "for"
    _ <- parseAmongWhitespace $ parseChar '('
    initialization <- parseAmongWhitespace parseForLoopInitialization
    _ <- parseAmongWhitespace $ parseChar ';'
    condition <- parseAmongWhitespace parseForLoopCondition
    _ <- parseAmongWhitespace $ parseChar ';'
    update <- parseAmongWhitespace parseForLoopUpdate
    _ <- parseAmongWhitespace $ parseChar ')'
    block <- parseAmongWhitespace parseBlock
    return $ ForLoopIter {forLoopInitialization=initialization, forLoopCondition=condition,
        forLoopUpdate=update, forLoopIterBlock=block}

-- | Parse initialization part of a for loop
parseForLoopInitialization :: Parser GomExpr
parseForLoopInitialization = parseVariableDeclaration <|> parseAssignent <|> parseExpression <|> pure Empty

-- | Parse condition part of a for loop
parseForLoopCondition :: Parser GomExpr
parseForLoopCondition = parseExpression

-- | Parse a value assigment or nothing (empty)
parseForLoopUpdate :: Parser GomExpr
parseForLoopUpdate = parseAssignent <|> pure Empty

-- | Print an expression
--parsePrint :: Parser String
--parsePrint = parseGomExprlSymbol "print" *> parseChar '(' *> parseExpression <* parseChar ')'

-- | Parse a boolean and return a GomExpr
parseBoolean :: Parser GomExpr
parseBoolean = do
    parsed <- parseSymbol "True" <|> parseSymbol "False"
    return (Boolean (parsed == "True"))

-- | parse until any of the given characters is found
parseUntilAny :: String -> Parser String
parseUntilAny toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right ("", str')
             | otherwise -> case runParser (parseUntilAny toFind) str' of
                Right (result, str'') -> Right (x:result, str'')
                Left err -> Left err
    [] -> Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

-- | parse comment after // to skip them
parseComment :: Parser String
parseComment = do
    _ <- parseChar '/' *> parseChar '/'
    parseUntilAny "\n"

-- Parse an if statement
parseCondition :: Parser GomExpr
parseCondition = do
    _ <- parseAmongWhitespace (parseSymbol "if")
    _ <- parseAmongWhitespace $ parseChar '('
    condition <- parseExpression
    _ <- parseAmongWhitespace $ parseChar ')'
    thenBlock <- parseAmongWhitespace parseBlock
    maybeElseBlock <- parseAmongWhitespace (parseElseBlock <|> pure Empty)
    return $ Condition {gomIfCondition=condition, gomIfTrue=thenBlock, gomIfFalse=maybeElseBlock}

-- Parse an else block
parseElseBlock :: Parser GomExpr
parseElseBlock = do
    _ <- parseAmongWhitespace (parseSymbol "else")
    elseBlock <- parseAmongWhitespace parseBlock
    return elseBlock


parseIncludeList :: Parser GomExpr
parseIncludeList = List <$> parseSep ',' (parseAmongWhitespace parseImportIdentifier)

-- Parse an include statement
parseIncludeStatement :: Parser GomExpr
parseIncludeStatement = do
    _ <- parseAmongWhitespace (parseSymbol "include")
    include <- parseAmongWhitespace parseImportIdentifier <|> parseAmongWhitespace parseIncludeList
    _ <- parseAmongWhitespace (parseSymbol "from")
    moduleName <- parseModule
    _ <- parseChar ';'
    return $ IncludeStatement {includeList=include, fromModule=moduleName}

parseModule :: Parser GomExpr
parseModule = parseIdentifier

parseImportIdentifier :: Parser GomExpr
parseImportIdentifier = parseIdentifier

-- parse a declare type by user
parseCustomType :: Parser GomExprType
parseCustomType = SingleType <$> parseToken

-- Parse the return identifier at the end of a fct
parseReturnType :: Parser GomExpr
parseReturnType = parseIdentifier

-- | parse everything between ( and ) and return a list of GomExpr
parseList :: Parser GomExpr
parseList = List <$> parseContent '(' ')' parseGomExpr

-- | Parser pour une déclaration de variable
parseVariableDeclaration :: Parser GomExpr
parseVariableDeclaration = do
    variableName <- parseAmongWhitespace $ parseIdentifier
    _ <- parseChar ':'
    variableType <- parseAmongWhitespace $ parseIdentifier
    _ <- parseChar '='
    variableValue <- parseAmongWhitespace $ parseIdentifier
    return $ Statements [Identifier "var", variableName, variableType, variableValue]

-- | parse everything between { and } and return a list of GomExpr
parseBlock :: Parser GomExpr
parseBlock = do
    _ <- parseChar '{'
    result <- parseAmongWhitespace $ parseMany (parseStatement)
    _ <- parseChar '}'
    return $ Block result
--parseBlock = Block<$> parseMany (parseVariableDeclaration <|> parseGomExpr)

-- | Parse name of functions
parseFunctionName :: Parser GomExpr
parseFunctionName = parseIdentifier

-- | Parse function declaration
-- parseFunctionDeclaration :: Parser GomExpr
-- parseFunctionDeclaration = parseSymbol "fn" *> parseAmongWhitespace *> parseFunctionName *> parseAmongWhitespace *> parseChar '(' *> parseAmongWhitespace *> parseParameterList *> parseAmongWhitespace *> parseChar ')' *> parseAmongWhitespace *> parseChar '-' *> parseSymbol "->" *> parseAmongWhitespace *> parseReturnType *> parseAmongWhitespace *> parseBlock

-- | Parser pour une déclaration de fonction
parseFunctionDeclaration :: Parser GomExpr
parseFunctionDeclaration = do
    _ <- parseAmongWhitespace parseIdentifier  -- Le mot-clé "fn"
    functionName <- parseAmongWhitespace parseIdentifier
    arguments <- parseAmongWhitespace $ parseList
    _ <- parseAmongWhitespace $ parseChar '-' *> parseChar '>'
    returnType <- parseAmongWhitespace $ parseIdentifier
    --_ <- parseAmongWhitespace $ parseChar '{'
    body <- parseAmongWhitespace $ parseBlock
    --_ <- parseAmongWhitespace $ parseChar '}'
    return $ Function {fnName=functionName, fnArguments=arguments, fnReturnType=returnType, fnBody=body}

-- | Parser pour un appel de fonction
parseFunctionCall :: Parser GomExpr
parseFunctionCall = do
    functionName <- parseIdentifier
    arguments <- parseAmongWhitespace $ parseList
    return $ FunctionCall {functionName=functionName, functionArguments=arguments}

parseAmongWhitespace :: Parser a -> Parser a
parseAmongWhitespace parser = do
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    result <- parser
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    return result

parseSemicolumn :: Parser a -> Parser a
parseSemicolumn parser = do
    result <- parser
    _ <- parseChar ';'
    return result

-- | Pour gérer une liste d'expressions
parseExpressionList :: Parser GomExpr
parseExpressionList = List <$> parseContent '(' ')' parseGomExpr

-- | Ensuite, vous pouvez combiner tous ces parsers pour gérer la structure de votre langage
parseGomExpr :: Parser GomExpr
parseGomExpr = parseFunctionDeclaration

-- | Parse code to return GomExpr
parseCodeToGomExpr :: Parser [GomExpr]
parseCodeToGomExpr = parseMany parseGomExpr

