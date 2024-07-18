{-# OPTIONS_GHC -Wno-partial-fields #-}

module Sirius.Syntax (
    Pass (..),
    Name (..),
    TagName,
    Program (..),
    Declaration (..),
    Command (..),
    GenericArgument (..),
    Entity (..),
    TargetSelector (..),
    SelectorArgument (..),
    TagProperties (..),
) where

import Relude

data Pass = Parsed | Resolved

data Name :: Pass -> Type where
    NamespacedName ::
        { namespace :: Text
        , name :: Text
        } ->
        Name p
    RawName :: Text -> Name p
    LocalName :: Text -> Name Parsed

data TagProperties = MkTagProperties
    { isLiteral :: Bool
    }

type TagName :: Pass -> Type
type family TagName p where
    TagName Parsed = Name Parsed
    TagName Resolved = (Name Resolved, TagProperties)

data Program (p :: Pass) = MkProgram
    { namespace :: Text
    , declarations :: Seq (Declaration p)
    }

data Declaration (p :: Pass)
    = DefineFunction Text (Seq (Command p))
    | DefineTag
        { tagName :: Text
        , literal :: Bool
        }

data Command p
    = GenericCommand Text (Seq (GenericArgument p))
    | Function (Name p)
    | TagAdd (Entity p) (TagName p)
    | TagRemove (Entity p) (TagName p)
    | Say Text

data Entity p
    = QuotedEntity Text
    | Selector TargetSelector (Seq (SelectorArgument p))

data TargetSelector
    = NearestPlayer
    | RandomPlayer
    | AllPlayers
    | AllEntities
    | Self
    | Nearest

data SelectorArgument p
    = TagSelector (TagName p)
    | GenericSelector Text (GenericArgument p)

data GenericArgument :: Pass -> Type where
    GenericTag :: (TagName Resolved) -> GenericArgument Resolved
    Named :: (Name p) -> GenericArgument p
    Literal :: Text -> GenericArgument p
    Lambda :: (Seq (Command p)) -> GenericArgument p
    Int :: Integer -> GenericArgument p
    GenericEntity :: Entity p -> GenericArgument p
