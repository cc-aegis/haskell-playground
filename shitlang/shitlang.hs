import Data.Char (ord)

data Token = Number Int
    | Ident String
    | Let
    | Assign
    | Semicolon
    | If
    | Then
    | Else
    | Inc
    | Dec

data Lexer = Lexer [Char]

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

skipWhitespace :: Lexer -> Lexer
skipWhitespace Lexer (' ':rest) = Lexer (skipWhitespace rest)
skipWhitespace lexer = lexer

parseNumber :: Lexer -> (Lexer, Int)
parseNumber Lexer (c:rest)
    | isDigit c -> let (lexer, num) = parseNumber (Lexer rest) in
        (lexer, ((ord c) - (ord '0')))
    | otherwise -> (Lexer (c:rest), 0)

nextToken :: Lexer -> Maybe (Lexer, Token)
nextToken lexer = case skip_whitespace lexer of
    Lexer [] -> Nothing
    Lexer c:rest | isDigit c -> parseNumber lexer
    TODO


tokenize :: Lexer -> [Token]
tokenize lexer = case nextToken lexer of
    Nothing -> []
    Just (lexer, token) -> token : (tokenize lexer) 

main :: IO ()
main = do