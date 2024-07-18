module Sirius.Parser (Sirius.Parser.parse) where

import Relude hiding (many)

import Sirius.Syntax

-- we need to override spaces to allow comments so we hide space to avoid accidentally calling the wrong function

import Relude.Unsafe (read)
import Text.Megaparsec as Megaparsec hiding (ParseError)
import Text.Megaparsec.Char as Char hiding (
    space,
 )
import Text.Megaparsec.Char qualified as Char

data ParseError deriving (Eq, Ord)

instance ShowErrorComponent ParseError where
    showErrorComponent = \case {}

type Parser a = Parsec ParseError Text a

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ParseError) (Program Parsed)
parse filename contents = do
    Megaparsec.parse program filename contents

many1 :: Parser a -> Parser [a]
many1 p = do
    first <- p
    rest <- many p
    pure (first : rest)

sepByTrailing :: Parser a -> Parser () -> Parser [a]
sepByTrailing parser separator = fix \recurse -> do
    choice @[]
        [ do
            item <- parser
            choice @[]
                [ do
                    separator
                    (item :) <$> recurse
                , pure [item]
                ]
        , pure []
        ]

spaces :: Parser ()
spaces = do
    Char.space
    option () $ label "comment" do
        _ <- chunk "//"
        _ <- many (satisfy (/= '\n'))
        spaces

keyword :: Text -> Parser ()
keyword text = spaces *> chunk text *> spaces

comma :: Parser ()
comma = keyword ","

semi :: Parser ()
semi = keyword ";"

lbrace :: Parser ()
lbrace = keyword "{"

rbrace :: Parser ()
rbrace = keyword "}"

-- TODO: I don't know if minecraft supports unicode at all so we might want to
-- restrict this to ASCII in the future
identNoSpaces :: Parser Text
identNoSpaces = do
    first <- letterChar <|> oneOf @[] "_"
    rest <- many (alphaNumChar <|> oneOf @[] "_")
    pure (toText (first : rest))

ident :: Parser Text
ident = label "identifier" $ spaces *> identNoSpaces <* spaces

integer :: Parser Integer
integer = do
    spaces
    negative <- optional (chunk "-")
    digits <- many1 digitChar
    case negative of
        Nothing -> pure (read digits)
        Just _ -> pure (-(read digits))

program :: Parser (Program Parsed)
program = do
    keyword "namespace"
    namespace <- ident
    semi
    declarations <- declaration `sepByTrailing` semi
    eof
    pure
        $ MkProgram
            { namespace
            , declarations = fromList declarations
            }

declaration :: Parser (Declaration Parsed)
declaration =
    label "declaration"
        $ choice @[]
            [ defineTag
            , defineFunction
            ]

defineTag :: Parser (Declaration Parsed)
defineTag = do
    keyword "tag"
    name <- ident
    literal <- option False (keyword "literal" *> pure True)
    pure $ DefineTag name literal

defineFunction :: Parser (Declaration Parsed)
defineFunction = do
    name <- ident
    lbrace
    commands <- command `sepByTrailing` semi
    rbrace
    pure (DefineFunction name (fromList commands))

command :: Parser (Command Parsed)
command = label "command" do
    choice @[]
        [ functionCommand
        , tagCommand
        , sayCommand
        , genericCommand
        ]

functionCommand :: Parser (Command Parsed)
functionCommand = do
    keyword "function"
    functionName <- name
    pure (Function functionName)

tagCommand :: Parser (Command Parsed)
tagCommand = do
    keyword "tag"
    entity <- entity
    choice @[]
        [ fmap (TagAdd entity) $ keyword "add" *> name
        , fmap (TagRemove entity) $ keyword "remove" *> name
        ]

sayCommand :: Parser (Command Parsed)
sayCommand = do
    keyword "say"
    message <- quoted
    pure (Say message)

genericCommand :: Parser (Command Parsed)
genericCommand = do
    command <- quoted
    arguments <- many genericArgument
    pure (GenericCommand command (fromList arguments))

genericArgument :: Parser (GenericArgument Parsed)
genericArgument =
    choice @[]
        [ Literal <$> quoted
        , GenericEntity <$> entity
        , Int <$> integer
        , Named <$> name
        , do
            lbrace
            commands <- command `sepByTrailing` semi
            rbrace
            pure (Lambda (fromList commands))
        ]

name :: Parser (Name Parsed)
name = label "name" do
    choice @[]
        [ RawName <$> quoted
        , do
            spaces
            firstName <- identNoSpaces
            rest <- optional do
                _ <- chunk ":"
                identNoSpaces
            spaces
            case rest of
                Nothing -> pure (LocalName firstName)
                Just secondName -> pure (NamespacedName firstName secondName)
        ]

-- TODO: string escapes
quoted :: Parser Text
quoted = do
    spaces
    _ <- chunk "\""
    contents <- many $ satisfy (/= '"')
    _ <- chunk "\""
    spaces
    pure (toText contents)

entity :: Parser (Entity Parsed)
entity =
    (QuotedEntity <$> quoted)
        <|> do
            target <- targetSelector
            arguments <- option [] do
                keyword "["
                arguments <- selectorArgument `sepByTrailing` comma
                keyword "]"
                pure (fromList arguments)
            pure (Selector target arguments)

targetSelector :: Parser TargetSelector
targetSelector = label "target selector" do
    choice @[]
        [ keyword "@p" *> pure NearestPlayer
        , keyword "@r" *> pure RandomPlayer
        , keyword "@a" *> pure AllPlayers
        , keyword "@e" *> pure AllEntities
        , keyword "@s" *> pure Self
        , keyword "@n" *> pure Nearest
        ]

selectorArgument :: Parser (SelectorArgument Parsed)
selectorArgument = do
    let simple tagName expected cont = do
            keyword tagName
            keyword "="
            result <- expected
            pure (cont result)

    choice @[]
        [ simple "tag" name TagSelector
        , do
            tagName <- ident <|> quoted
            keyword "="
            argument <- genericArgument
            pure (GenericSelector tagName argument)
        ]
