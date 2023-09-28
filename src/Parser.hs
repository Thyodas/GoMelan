module Parser where
import Control.Applicative (Alternative(..))

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
    _ <- parseMany (parseChar ' ')
    left <- parser
    _ <- parseMany (parseChar ' ')
    right <- parser
    _ <- parseMany (parseChar ' ')
    _ <- parseChar ')'
    return (left, right)

parseList :: Parser a -> Parser [a]
parseList parser = do
    _ <- parseChar '('
    _ <- parseMany (parseChar ' ')
    result <- parseMany (parser <* parseMany (parseChar ' '))
    _ <- parseChar ')'
    return result
