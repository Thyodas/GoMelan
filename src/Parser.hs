{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-STG-5-1-glados-marie.giacomel [WSL: Ubuntu]
-- File description:
-- Parser
-}

module Parser (
    ErrorMsg, parseCodeToSExpr, Parser(..), parseChar, parseAnyChar, parseOr,
    parseAnd, parseAndWith, parseMany, parseSome, parseInt, parsePair, parseList
) where
import Control.Applicative (Alternative(..))
import Ast (SExpr(..))

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

parserWhitespaceChar :: String
parserWhitespaceChar = " \n\t"

parserTokenChar :: String
parserTokenChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_+-*/<>=?"

parseChar :: Char -> Parser Char
parseChar char = Parser $ \str -> case str of
    (x:str') | x == char -> Right (char, str')
             | otherwise -> Left ("Expected '" ++ [char]
                ++ "' but got '" ++ [x] ++ "'.")
    [] -> Left ("Expected '" ++ [char] ++ "' but got empty string.")

parseAnyChar :: String -> Parser Char
parseAnyChar toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right (x, str')
             | otherwise -> Left ("Expected one of '" ++ toFind
                ++ "' but got '" ++ [x] ++ "'.")
    [] -> Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 = Parser $ \str -> case runParser parser1 str of
    Left _ -> runParser parser2 str
    other -> other

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 = (,) <$> parser1 <*> parser2

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func parser1 parser2 = func <$> parser1 <*> parser2

parseMany :: Parser a -> Parser [a]
parseMany parser = (:) <$> parser <*> parseMany parser <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome parser = (:) <$> parser <*> parseMany parser

parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

parseInt :: Parser Int
parseInt = negate <$> (parseChar '-' *> parseUInt)
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

parseList :: Parser a -> Parser [a]
parseList parser = do
    _ <- parseChar '('
    _ <- parseMany (parseAnyChar " \n\t")
    result <- parseSome (parser <* parseMany
        (parseAnyChar parserWhitespaceChar))
    _ <- parseChar ')'
    return result

parseSymbol :: Parser SExpr
parseSymbol = Symbol <$> parseSome (parseAnyChar parserTokenChar)

parseNumber :: Parser SExpr
parseNumber = Number <$> parseInt

parseBoolean :: Parser SExpr
parseBoolean = do
    _ <- parseChar '#'
    parsed <- parseAnyChar "tf"
    return (Boolean (parsed == 't'))

parseAtom :: Parser SExpr
parseAtom =  parseBoolean <|> parseNumber <|> parseSymbol

-- parse until any of the given characters is found
parseUntilAny :: String -> Parser String
parseUntilAny toFind = Parser $ \str -> case str of
    (x:str') | x `elem` toFind -> Right ("", str)
             | otherwise -> case runParser (parseUntilAny toFind) str' of
                Right (result, str'') -> Right (x:result, str'')
                Left err -> Left err
    [] -> Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

parseComment :: Parser String
parseComment = do
    _ <- parseChar ';'
    parseUntilAny "\n"

parseSExpr :: Parser SExpr
parseSExpr = do
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    _ <- parseMany (parseComment)
    parsed <- parseAtom <|> List <$> parseList parseSExpr
    _ <- parseMany (parseAnyChar parserWhitespaceChar)
    _ <- parseMany (parseComment)
    return parsed

parseCodeToSExpr :: Parser [SExpr]
parseCodeToSExpr = parseMany parseSExpr
