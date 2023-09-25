module Parser where
import Data.IntMap (restrictKeys)
import Control.Monad.ST.Lazy (strictToLazyST)

type ErrorMsg = String

type Parser a = String -> Either ErrorMsg (a, String)

parseChar :: Char -> Parser Char
parseChar char (x:str)  | x == char = Right (char, str)
                        | otherwise = Left ("Expected '" ++ [char]
                            ++ "' but got '" ++ [x] ++ "'.")
parseChar char [] = Left ("Expected '" ++ [char] ++ "' but got empty string.")

parseAnyChar :: String -> Parser Char
parseAnyChar toFind (x:str) | x `elem` toFind = Right (x, str)
                            | otherwise = Left ("Expected one of '" ++ toFind
                                ++ "' but got '" ++ [x] ++ "'.")
parseAnyChar toFind [] = Left ("Expected one of '" ++ toFind ++ "' but got empty string.")

parseOr :: Parser a -> Parser a -> Parser a
parseOr parser1 parser2 str = case parser1 str of
    Left _ -> parser2 str
    other -> other

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd parser1 parser2 str = case parser1 str of
    Right (result1, str1) -> case parser2 str1 of
        Right (result2, str2) -> Right ((result1, result2), str2)
        Left err -> Left err
    Left err -> Left err

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func parser1 parser2 str = case parseAnd parser1 parser2 str of
    Right ((x, y), str') -> Right (func x y, str')
    Left err -> Left err

parseMany :: Parser a -> Parser [a]
parseMany _ "" = Right ([], "")
parseMany parser str = case parser str of
    Right (acc, rest) -> parseMany parser rest >>= (\(acc', rest') -> Right (acc : acc', rest'))
    Left _ -> Right ([], str)

parseSome :: Parser a -> Parser [a]
parseSome parser = parseAndWith (:) parser (parseMany parser)

parseUInt :: Parser Int
parseUInt str = parseSome (parseAnyChar ['0'..'9']) str >>= (\(digits, str') -> Right (read digits, str'))

parseInt :: Parser Int
parseInt str = case parseOr (parseChar '-') (parseChar '+') str of
    Right ('-', rest) -> parseUInt rest >>= (\(acc', rest') -> Right (acc' * (-1), rest'))
    Right ('+', rest) -> parseUInt rest
    _ -> parseUInt str
