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
--     parseUntilAny, parseComment, parseGomExpr, parseBody
-- ) where
module Parser where
import Control.Applicative (Alternative(..))
import Ast (GomExpr(..))

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

-- | Parse between x until x
parseBetween :: Char -> Char -> Parser String
parseBetween a b = parseChar a *> parseUntilAny b

-- | Parse characters bewteen " and "
parseString :: Parser String
parseString = parseBetween '"' '"'

-- Parse a return statement
parseReturnStatement :: Parser GomExpr
parseReturnStatement = do
    _ <- parseSymbol "return"
    expression <- parseExpression
    _ <- parseChar ';'
    return expression

-- Parse an expression
parseExpression :: Parser GomExpr
parseExpression = parseTermWithOperator <|> parseTermWithoutOperator

-- Parse a term with a binary operator and another expression
parseTermWithOperator :: Parser GomExpr
parseTermWithOperator = do
    term <- parseTerm
    operator <- parseBinaryOperator
    expression <- parseExpression
    return $ case operator of
        Symbol "+" -> Function (Symbol "+") term expression
        Symbol "-" -> Function (Symbol "-") term expression
        Symbol "*" -> Function (Symbol "*") term expression
        Symbol "/" -> Function (Symbol "/") term expression
        _          -> expression

-- Parse a term without a binary operator
parseTermWithoutOperator :: Parser GomExpr
parseTermWithoutOperator = parseTerm

-- Parse a term
parseTerm :: Parser GomExpr
parseTerm = parseFactorWithOperator <|> parseFactorWithoutOperator

-- Parse a factor with a binary operator and another term
parseFactorWithOperator :: Parser GomExpr
parseFactorWithOperator = do
    factor <- parseFactor
    operator <- parseBinaryOperator
    term <- parseTerm
    return $ case operator of
        Symbol "+" -> Function (Symbol "+") factor term
        Symbol "-" -> Function (Symbol "-") factor term
        Symbol "*" -> Function (Symbol "*") factor term
        Symbol "/" -> Function (Symbol "/") factor term
        _          -> term

-- Parse a factor without a binary operator
parseFactorWithoutOperator :: Parser GomExpr
parseFactorWithoutOperator = parseFactorfldnGlorenljbBHgre

parseFactor :: 

handleOtherCases :: Parser String
handleOtherCases = Parser $ \str -> Left ("Expected a binary operator, but got '" ++ take 1 str ++ "'.")

parseBinaryOperator :: Parser GomExpr
parseBinaryOperator = Operator <$> (parsePlus <|> parseMinus <|> parseMultiply <|> parseDivide <|> parseEqual <|> parseNotEqual <|> handleOtherCases)

parseSymbol :: String -> Parser String
parseSymbol [] -> Left ("Expected '" ++ [char] ++ "' but got empty string.")
parseSymbol [x] = parseChar x
parseSymbol (x:xs) = parseChar x <*> parseSymbol xs

parseOperatorPlus :: Parser String
parseOperatorPlus = parseSymbol "+"

parseOperatorMinus :: Parser String
parseOperatorMinus = parseSymbol "-"

parseOperatorMultiply :: Parser String
parseOperatorMultiply = parseSymbol "*"

parseOperatorDivide :: Parser String
parseOperatorDivide = parseSymbol "/"

parseOperatorEqual :: Parser String
parseOperatorEqual = parseSymbol "=="

parseOperatorNotEqual :: Parser String
parseOperatorNotEqual = parseSymbol "!="

parseOperatorNot :: Parser String
parseOperatorNot :: parseSymbol "!"

parseOperatorAnd :: Parser String
parseOperatorAnd :: parseSymbol "&&"

-- | Parse specific char of a string passed in arg and return a parser return a parser
parseAnyChar :: String -> Parser Char
parseAnyChar toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right (x, str')
             | otherwise -> Left ("Expected one of '" ++ toFind
                ++ "' but got '" ++ [x] ++ "'.")
    [] -> Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

-- |
ParseLiteral :: Parser
ParseLiteral = parseNumber <|> parseString <|> parseBoolean

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

-- | Parse a symbol as string and return a GomExpr
parseIdentifier :: Parser GomExpr
parseIdentifier = Symbol <$> parseSome (parseAnyChar parserTokenChar)

-- | Parse a number and return a GomExpr
parseNumber :: Parser GomExpr
parseNumber = Number <$> parseInt

-- | Parse a boolean and return a GomExpr
parseBoolean :: Parser GomExpr
parseBoolean = do
    parsed <- parseSymbol "True" <|> parseSymbol "False"
    return (Boolean (parsed == "True"))

-- | parse Atom (bool / Number / Symbol) and return a GomExpr
parseAtom :: Parser GomExpr
parseAtom =  parseBoolean <|> parseNumber <|> parseIdentifier

-- | parse multiple Atoms (bool / Number / Symbol) and return a list of GomExprl
parseMultipleAtom :: Parser GomExpr
parseMultipleAtom = Statements <$> parseSome parseAtom

-- | parse until any of the given characters is found
parseUntilAny :: String -> Parser String
parseUntilAny toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right ("", str)
             | otherwise -> case runParser (parseUntilAny toFind) str' of
                Right (result, str'') -> Right (x:result, str'')
                Left err -> Left err
    [] -> Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

-- | parse comment after // to skip them
parseComment :: Parser String
parseComment = do
    _ <- parseChar '/' *> parseChar '/'
    parseUntilAny "\n"

-- | parse everything between ( and ) and return a list of GomExpr
parseList :: Parser GomExpr
parseList = List <$> parseContent '(' ')' parseGomExpr

-- Parser pour une déclaration de variable
parseVariableDeclaration :: Parser GomExpr
parseVariableDeclaration = do
    variableName <- parseAmongWhitespace $ parseIdentifier
    _ <- parseChar ':'
    variableType <- parseAmongWhitespace $ parseIdentifier
    _ <- parseChar '='
    variableValue <- parseAmongWhitespace $ parseIdentifier
    _ <- parseChar ';'
    return $ Statements [Symbol "var", variableName, variableType, variableValue]

-- | parse everything between { and } and return a list of GomExpr
parseBody :: Parser GomExpr
parseBody = do
    _ <- parseChar '{'
    result <- parseAmongWhitespace $ parseMany (parseVariableDeclaration <|> parseGomExpr)
    _ <- parseChar '}'
    return $ Body result
--parseBody = Body <$> parseMany (parseVariableDeclaration <|> parseGomExpr)

-- Parser pour une déclaration de fonction
parseFunctionDeclaration :: Parser GomExpr
parseFunctionDeclaration = do
    _ <- parseAmongWhitespace parseIdentifier  -- Le mot-clé "fn"
    functionName <- parseAmongWhitespace parseIdentifier
    arguments <- parseAmongWhitespace $ parseList
    _ <- parseAmongWhitespace $ parseChar '-' *> parseChar '>'
    returnType <- parseAmongWhitespace $ parseIdentifier
    --_ <- parseAmongWhitespace $ parseChar '{'
    body <- parseAmongWhitespace $ parseBody
    --_ <- parseAmongWhitespace $ parseChar '}'
    return $ Function {fnName=functionName, fnArguments=arguments, fnReturnType=returnType, fnBody=body}

-- | Parser pour un appel de fonction
parseFunctionCall :: Parser GomExpr
parseFunctionCall = do
    functionName <- parseIdentifier
    _ <- parseChar '('
    -- Parsez les arguments ici (vous devrez créer un parser spécifique pour cela)
    _ <- parseChar ')'
    return $ Statements [functionName]  -- Vous pouvez ajouter les arguments ici

parseAmongWhitespace :: Parser a -> Parser a
parseAmongWhitespace parser = do
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    result <- parser
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    return result

-- | Parser pour la boucle "for"
parseForLoop :: Parser GomExpr
parseForLoop = do
    _ <- parseIdentifier  -- Le mot-clé "for"
    _ <- parseChar '('
    loopVariable <- parseIdentifier
    _ <- parseChar ':'
    startValue <- parseNumber
    _ <- parseChar ';'
    endCondition <- parseIdentifier  -- Vous devrez créer un parser pour les conditions
    _ <- parseChar ';'
    _ <- parseIdentifier  -- L'incrémentation (par exemple, "i++")
    _ <- parseChar ')'
    body <- parseBody
    return $ Statements [Symbol "for", loopVariable, startValue, endCondition, body]

-- | Pour gérer une liste d'expressions
parseExpressionList :: Parser GomExpr
parseExpressionList = List <$> parseContent '(' ')' parseGomExpr

-- | Ensuite, vous pouvez combiner tous ces parsers pour gérer la structure de votre langage
parseGomExpr :: Parser GomExpr
parseGomExpr = parseFunctionDeclaration

-- | Parse code to return GomExpr
parseCodeToGomExpr :: Parser [GomExpr]
parseCodeToGomExpr = parseMany parseGomExpr

