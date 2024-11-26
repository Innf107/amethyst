module Sirius.Parser (Sirius.Parser.parse) where

import Relude hiding (Ordering (..), many)

import Sirius.Syntax

-- we need to override spaces to allow comments so we hide space to avoid accidentally calling the wrong function

import Data.Sequence ((|>))
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
            [ defineFunction
            , defineTag
            , definePlayer
            , defineObjective
            ]

defineFunction :: Parser (Declaration Parsed)
defineFunction = do
    keyword "function"
    name <- ident
    lbrace
    commands <- command `sepByTrailing` semi
    rbrace
    pure (DefineFunction name (fromList commands))

defineTag :: Parser (Declaration Parsed)
defineTag = do
    keyword "tag"
    name <- ident
    literal <- option False (keyword "literal" *> pure True)
    pure $ DefineTag name literal

definePlayer :: Parser (Declaration Parsed)
definePlayer = do
    keyword "player"
    name <- ident
    pure $ DefinePlayer name

defineObjective :: Parser (Declaration Parsed)
defineObjective = do
    keyword "objective"
    name <- ident
    literal <- option False (keyword "literal" *> pure True)
    pure $ DefineObjective name literal

command :: Parser (Command Parsed)
command = label "command" do
    choice @[]
        [ functionCommand
        , tagCommand
        , sayCommand
        , executeCommand
        , genericCommand
        ]

functionCommand :: Parser (Command Parsed)
functionCommand = do
    keyword "function"

    choice @[]
        [ FunctionName <$> name
        , FunctionLambda <$> lambda
        ]

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

executeCommand :: Parser (Command Parsed)
executeCommand = do
    keyword "execute"
    [] & fix \recurse clauses ->
        choice @[]
            [ keyword "run" >> ExecuteRun clauses <$> command
            , do
                clause <- executeClause
                recurse (clauses |> clause)
            ]

executeClause :: Parser (ExecuteClause Parsed)
executeClause = do
    choice @[]
        [ QuotedClause <$> quoted
        , keyword "anchored" >> Anchored <$> anchorPoint
        , keyword "as" >> As <$> entity
        , keyword "at" >> As <$> entity
        , keyword "facing"
            >> choice @[]
                [ keyword "entity" >> FacingEntity <$> entity <*> anchorPoint
                , Facing <$> position
                ]
        , keyword "if" >> executeIfClause
        , keyword "in" >> In <$> dimension
        , keyword "positioned"
            >> choice @[]
                [ keyword "as" >> PositionedAs <$> entity
                , Positioned <$> position
                ]
        , keyword "rotated"
            >> choice @[]
                [ keyword "as" >> RotatedAs <$> entity
                ]
        , keyword "summon" >> Summon <$> quoted
        ]

executeIfClause :: Parser (ExecuteClause Parsed)
executeIfClause =
    choice @[]
        [ keyword "biome" >> undefined
        , keyword "block" >> undefined
        , keyword "blocks" >> undefined
        , keyword "data" >> undefined
        , keyword "score" >> do
            target1 <- scoreTarget
            objective1 <- name
            choice @[]
                [ "matches" >> do
                    range <- scoreRange
                    pure (IfScoreMatches target1 objective1 range)
                , do
                    comparison <-
                        choice @[]
                            [ keyword "<=" $> LE
                            , keyword "<" $> LT
                            , keyword "=" $> EQ
                            , keyword ">=" $> GE
                            , keyword ">" $> GT
                            ]
                    target2 <- scoreTarget
                    objective2 <- name
                    pure (IfScore target1 objective1 comparison target2 objective2)
                ]
        ]

scoreTarget :: Parser (ScoreTarget Parsed)
scoreTarget =
    choice @[]
        [ PlayerScore <$> playerName
        , EntityScore <$> entity
        ]

playerName :: Parser PlayerName
playerName =
    choice @[]
        [ QuotedPlayer <$> quoted
        , PlayerName <$> ident
        ]

scoreRange :: Parser ScoreRange
scoreRange = do
    start <- integer
    end <- optional $ try $ keyword ".." >> integer
    case end of
        Just end -> pure $ MkScoreRange start end
        Nothing -> pure $ MkScoreRange start start

dimension :: Parser Dimension
dimension =
    choice @[]
        [ keyword "minecraft:overworld" *> pure Overworld
        , keyword "minecraft:nether" *> pure Nether
        , keyword "minecraft:end" *> pure End
        ]

anchorPoint :: Parser AnchorPoint
anchorPoint =
    choice @[]
        [ keyword "feet" *> pure Feet
        , keyword "eyes" *> pure Eyes
        ]

position :: Parser Position
position = undefined

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
        , Lambda <$> lambda
        ]

lambda :: Parser (Seq (Command Parsed))
lambda = do
    lbrace
    commands <- command `sepByTrailing` semi
    rbrace
    pure (fromList commands)

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
