{-# OPTIONS_GHC -Wno-partial-fields #-}

module Amethyst.Syntax (
    Pass (..),
    Name (..),
    TagName,
    Program (..),
    Declaration (..),
    Command (..),
    Function (..),
    GenericArgument (..),
    Operation (..),
    ExecuteClause (..),
    IfCondition (..),
    ScoreTarget (..),
    PlayerName (..),
    Staged (..),
    StagedType (..),
    ObjectiveName (..),
    Range (..),
    StoredValue (..),
    StoreLocation (..),
    ScoreComparison (..),
    AnchorPoint (..),
    Position (..),
    Dimension (..),
    Entity (..),
    TargetSelector (..),
    SelectorArgument (..),
    TagProperties (..),
    ObjectiveProperties (..),
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

data ObjectiveProperties = MkObjectiveProperties
    { isLiteral :: Bool
    }

type TagName :: Pass -> Type
type family TagName p where
    TagName Parsed = Name Parsed
    TagName Resolved = (Name Resolved, TagProperties)

type ObjectiveName :: Pass -> Type
type family ObjectiveName p where
    ObjectiveName Parsed = Name Parsed
    ObjectiveName Resolved = (Name Resolved, ObjectiveProperties)

type PassSpecific :: Pass -> Type -> Type -> Type
type family PassSpecific pass parsed resolved where
    PassSpecific Parsed parsed resolved = parsed
    PassSpecific Resolved parsed resolved = resolved

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
    | DefinePlayer Text
    | DefineObjective
        { objectiveName :: PassSpecific p Text (ObjectiveName Resolved)
        , literal :: Bool
        }
    | DefineSearchTree
        { name :: Text
        , rangeStart :: Staged p
        , rangeEnd :: Staged p
        , target :: ScoreTarget p
        , objective :: ObjectiveName p
        , varName :: Text
        , body :: Seq (Command p)
        }

data Command p
    = GenericCommand Text (Seq (GenericArgument p))
    | Function (Function p)
    | TagAdd (Entity p) (TagName p)
    | TagRemove (Entity p) (TagName p)
    | ExecuteRun (Seq (ExecuteClause p)) (Command p)
    | ExecuteIf (Seq (ExecuteClause p)) -- TODO
    | Say (Staged p)
    | ReturnValue (Staged p)
    | ReturnFail
    | ReturnRun (Command p)
    | ScoreboardPlayersGet (ScoreTarget p) (ObjectiveName p)
    | ScoreboardPlayersSet (ScoreTarget p) (ObjectiveName p) (Staged p)
    | ScoreboardPlayersAdd (ScoreTarget p) (ObjectiveName p) (Staged p)
    | ScoreboardPlayersOperation (ScoreTarget p) (ObjectiveName p) Operation (ScoreTarget p) (ObjectiveName p)

data Operation
    = Assign
    | Min
    | Max
    | Swap
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod

data ExecuteClause p
    = QuotedClause Text
    | Align Void -- TODO
    | Anchored AnchorPoint
    | As (Entity p)
    | At (Entity p)
    | Facing Position
    | FacingEntity (Entity p) AnchorPoint
    | If (IfCondition p)
    | Unless (IfCondition p)
    | In Dimension
    | On Void -- TODO (but really interesting)
    | PositionedAs (Entity p)
    | PositionedOver Void -- TODO
    | Positioned Position
    | RotatedAs (Entity p)
    | Rotated Void -- TODO (position but with only two coordinates)
    | Store StoredValue (StoreLocation p)
    | Summon Text

data StoredValue
    = Result
    | Success

data StoreLocation p
    = StoreBlock Void
    | StoreBossbar Void
    | StoreEntity Void
    | StoreStorage Void
    | StoreScore (ScoreTarget p) (ObjectiveName p)

data IfCondition p
    = IfBiome Void -- TODO
    | IfBlock Void -- TODO
    | IfBlocks Void -- TODO
    | IfData Void -- TODO
    | IfEntity (Entity p)
    | IfFunction (Function p)
    | IfScoreMatches (ScoreTarget p) (ObjectiveName p) (Range p)
    | IfScore (ScoreTarget p) (ObjectiveName p) ScoreComparison (ScoreTarget p) (ObjectiveName p)

data Function p
    = FunctionName (Name p)
    | FunctionLambda (Seq (Command p))

data ScoreComparison = LT | LE | EQ | GE | GT

data ScoreTarget p
    = EntityScore (Entity p)
    | PlayerScore PlayerName

data PlayerName
    = PlayerName Text
    | QuotedPlayer Text

data Staged p
    = StagedInt Integer
    | StagedVar Text
    | StagedQuote Text

data StagedType = IntT | AnyT deriving (Show)

data Range p = MkRange (Staged p) (Staged p)

data Dimension = Overworld | Nether | End

data AnchorPoint = Eyes | Feet

data Position

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
    | DistanceSelector (Range p)
    | GenericSelector Text (GenericArgument p)

data GenericArgument :: Pass -> Type where
    GenericTag :: (TagName Resolved) -> GenericArgument Resolved
    Named :: (Name p) -> GenericArgument p
    Literal :: Text -> GenericArgument p
    Lambda :: (Seq (Command p)) -> GenericArgument p
    Int :: Integer -> GenericArgument p
    GenericEntity :: Entity p -> GenericArgument p
