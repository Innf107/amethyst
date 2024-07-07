module Sirius.Lexer (LexicalError (..), Token (..), runLexer) where

import Data.Char qualified as Char
import Data.Text qualified as Text
import Relude
import Relude.Unsafe (read)

data LexicalError
    = UnexpectedChar Char
    | UnclosedQuote
    deriving (Show)

data Token
    = Ident Text
    | Quoted Text
    | Integer Integer
    | Semi
    | LBrace
    | RBrace
    | EOF

newtype Lexer a = MkLexer (ExceptT LexicalError (State Text) a)
    deriving (Functor, Applicative, Monad)

runLexer :: Text -> Either LexicalError [Token]
runLexer text = do
    let (MkLexer exceptT) = fix \repeat -> do
            lex >>= \case
                EOF -> pure []
                token -> (token :) <$> repeat
    evalState (runExceptT exceptT) text

lexicalError :: LexicalError -> Lexer a
lexicalError err = MkLexer (ExceptT (pure (Left err)))

peek :: Lexer (Maybe Char)
peek = do
    text <- MkLexer get
    case Text.uncons text of
        Nothing -> pure Nothing
        Just (char, _) -> pure (Just char)

consume :: Lexer ()
consume = do
    text <- MkLexer get
    case Text.uncons text of
        Nothing -> pure ()
        Just (_, rest) -> MkLexer $ put rest

lex :: Lexer Token
lex = do
    peek >>= \case
        Nothing -> pure EOF
        Just '#' -> consume >> lexLineComment
        Just ';' -> consume >> pure Semi
        Just '{' -> consume >> pure LBrace
        Just '}' -> consume >> pure RBrace
        Just '"' -> consume >> lexQuote '"' []
        Just '\'' -> consume >> lexQuote '\'' []
        Just char
            | Char.isSpace char -> consume >> lex
            | Char.isDigit char -> consume >> lexInteger [char]
            | isIdentStart char -> consume >> lexIdent [char]
        Just char -> lexicalError (UnexpectedChar char)

lexLineComment :: Lexer Token
lexLineComment = do
    peek >>= \case
        Nothing -> consume >> lex
        Just '\n' -> consume >> lex
        Just _ -> consume >> lexLineComment

lexQuote :: Char -> [Char] -> Lexer Token
lexQuote quoteChar chars = do
    peek >>= \case
        Nothing -> lexicalError (UnclosedQuote)
        Just char
            | char == quoteChar -> consume >> pure (Quoted (toText (reverse chars)))
            | otherwise -> consume >> lexQuote quoteChar (char : chars)

lexInteger :: [Char] -> Lexer Token
lexInteger chars = do
    let finish chars = pure (Integer (read (reverse chars)))
    peek >>= \case
        Nothing -> finish chars
        Just char
            | Char.isDigit char -> consume >> lexInteger (char : chars)
            | otherwise -> finish chars

lexIdent :: [Char] -> Lexer Token
lexIdent chars = do
    let finish chars = pure (Ident (toText (reverse chars)))
    peek >>= \case
        Nothing -> finish chars
        Just char
            | isIdentLetter char -> consume >> lexIdent (char : chars)
            | otherwise -> finish chars

isIdentStart :: Char -> Bool
isIdentStart char = Char.isAlpha char || char == '_'

isIdentLetter :: Char -> Bool
isIdentLetter char = Char.isAlphaNum char || char == '_'