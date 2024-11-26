module Sirius.Compile (runCompile) where

import Relude hiding (Ordering (..))

import Sirius.Syntax

import Data.Sequence ((<|))
import Data.Text qualified as Text
import Data.Unique
import System.FilePath ((</>))

-- TODO: use a proper text builder that isn't quadratic
data CompileEnv = MkCompileEnv
    { fileWriter :: FilePath -> Text -> IO ()
    , namespace :: Text
    , initializationFileContents :: IORef Text
    }

newtype Compile a = MkCompile (ReaderT CompileEnv IO a)
    deriving newtype (Functor, Applicative, Monad)

currentNamespace :: Compile Text
currentNamespace = MkCompile do
    MkCompileEnv{namespace} <- ask
    pure namespace

freshName :: Text -> Compile (Name Resolved)
freshName base = MkCompile do
    MkCompileEnv{namespace} <- ask

    unique <- liftIO newUnique
    pure (NamespacedName{namespace, name = base <> "_" <> show (hashUnique unique)})

runCompile :: Program Resolved -> (FilePath -> Text -> IO ()) -> IO ()
runCompile program fileWriter = do
    initializationFileContents <- newIORef ""
    let env =
            MkCompileEnv
                { fileWriter
                , namespace = program.namespace
                , initializationFileContents
                }
    let (MkCompile readerT) = compile program

    readerT `runReaderT` env
    finalInitFile <- readIORef initializationFileContents
    fileWriter ("data" </> toString program.namespace </> "function/init.mcfunction") finalInitFile
    fileWriter ("data/minecraft/tags/function/load.json") ("{\"values\":[\"" <> program.namespace <> ":init\"]}")

emitNamespacedFile :: FilePath -> Text -> Compile ()
emitNamespacedFile file contents = MkCompile do
    MkCompileEnv{fileWriter, namespace} <- ask
    liftIO $ fileWriter ("data" </> toString namespace </> file) contents

emitInitCommand :: Text -> Compile ()
emitInitCommand command = MkCompile do
    MkCompileEnv{initializationFileContents} <- ask
    modifyIORef' initializationFileContents (<> command <> "\n")

compile :: Program Resolved -> Compile ()
compile program = do
    traverse_ compileDeclaration program.declarations

compileDeclaration :: Declaration Resolved -> Compile ()
compileDeclaration = \case
    DefineFunction name commands -> do
        namespace <- currentNamespace
        compileFunction (NamespacedName namespace name) commands
    DefineTag{} -> pure ()
    DefinePlayer{} -> pure ()
    DefineObjective{objectiveName} -> do
        emitInitCommand ("scoreboard objectives add " <> renderObjectiveName objectiveName <> " dummy")

compileFunction :: Name Resolved -> Seq (Command Resolved) -> Compile ()
compileFunction name commands = do
    commandTexts <- traverse compileCommand commands
    emitNamespacedFile ("function" </> toString name.name <> ".mcfunction") (Text.intercalate "\n" (toList commandTexts))

compileCommand :: Command Resolved -> Compile Text
compileCommand = \case
    GenericCommand name arguments -> do
        argumentTexts <- traverse compileGenericArgument arguments
        pure (unwords (toList (name <| argumentTexts)))
    FunctionName name ->
        pure $ "function " <> renderName name
    FunctionLambda commands ->
        ("function " <>) <$> compileLambda commands
    TagAdd entity tagName -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " add " <> renderTagName tagName
    TagRemove entity tagName -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " remove " <> renderTagName tagName
    Say message -> pure $ "say " <> message
    ExecuteRun clauses command -> do
        clauses <- traverse compileExecuteClause clauses
        command <- compileCommand command
        pure $ "execute " <> Text.intercalate " " (toList (clauses <> ["run " <> command]))
    ExecuteIf{} -> undefined

compileExecuteClause :: ExecuteClause Resolved -> Compile Text
compileExecuteClause = \case
    QuotedClause text -> pure text
    Anchored anchorPoint -> pure $ "anchored " <> renderAnchorPoint anchorPoint
    As entity -> ("as " <>) <$> compileEntity entity
    At entity -> ("at " <>) <$> compileEntity entity
    Facing position -> pure $ "facing " <> renderPosition position
    FacingEntity entity anchorPoint -> do
        entity <- compileEntity entity
        pure ("facing entity " <> entity <> " " <> renderAnchorPoint anchorPoint)
    IfScoreMatches target objective range -> do
        target <- compileScoreTarget target
        range <- compileScoreRange range
        pure ("if score " <> target <> " " <> renderObjectiveName objective <> " matches " <> range)
    IfScore target1 objective1 comparison target2 objective2 -> do
        target1 <- compileScoreTarget target1
        target2 <- compileScoreTarget target2
        comparison <- pure $ case comparison of
            LT -> "<"
            LE -> "<="
            EQ -> "="
            GE -> ">="
            GT -> ">"
        pure
            ( "if score "
                <> target1
                <> " "
                <> renderObjectiveName objective1
                <> " "
                <> comparison
                <> " "
                <> target2
                <> " "
                <> renderObjectiveName objective2
            )
    In dimension -> pure $ "in " <> renderDimension dimension
    PositionedAs entity -> ("positioned as " <>) <$> compileEntity entity
    Positioned position -> pure $ "positioned " <> renderPosition position
    RotatedAs entity -> ("rotated as " <>) <$> compileEntity entity
    Summon text -> pure $ "summon " <> text

compileScoreTarget :: ScoreTarget Resolved -> Compile Text
compileScoreTarget = \case
    EntityScore entity -> compileEntity entity
    PlayerScore playerName -> pure (renderPlayerName playerName)

compileScoreRange :: ScoreRange -> Compile Text
compileScoreRange (MkScoreRange start end) = pure $ show start <> ".." <> show end

renderDimension :: Dimension -> Text
renderDimension = \case
    Overworld -> "minecraft:overworld"
    Nether -> "minecraft:nether"
    End -> "minecraft:end"

renderPosition :: Position -> Text
renderPosition = \case {}

renderAnchorPoint :: AnchorPoint -> Text
renderAnchorPoint = \case
    Eyes -> "eyes"
    Feet -> "feet"

compileGenericArgument :: GenericArgument Resolved -> Compile Text
compileGenericArgument = \case
    GenericTag tagName -> pure (renderTagName tagName)
    Named name -> pure (renderName name)
    Literal text -> pure text
    Lambda commands -> compileLambda commands
    Int int -> pure (show int)
    GenericEntity entity -> compileEntity entity

compileLambda :: Seq (Command Resolved) -> Compile Text
compileLambda commands = do
    name <- freshName "generated/f"
    compileFunction name commands
    pure $ renderName name

compileEntity :: Entity Resolved -> Compile Text
compileEntity = \case
    QuotedEntity text -> pure text
    Selector targetSelector selectorArguments -> do
        selector <- compileTargetSelector targetSelector
        arguments <- traverse compileSelectorArgument selectorArguments
        case arguments of
            [] -> pure selector
            _ -> pure (selector <> "[" <> Text.intercalate ", " (toList arguments) <> "]")

compileTargetSelector :: TargetSelector -> Compile Text
compileTargetSelector = \case
    NearestPlayer -> pure "@p"
    RandomPlayer -> pure "@r"
    AllPlayers -> pure "@a"
    AllEntities -> pure "@e"
    Self -> pure "@s"
    Nearest -> pure "@n"

compileSelectorArgument :: SelectorArgument Resolved -> Compile Text
compileSelectorArgument = \case
    TagSelector name -> pure $ "tag = " <> renderTagName name
    GenericSelector selectorName argument -> do
        argument <- compileGenericArgument argument
        pure (selectorName <> " = " <> argument)

renderName :: Name Resolved -> Text
renderName = \case
    NamespacedName namespace name -> namespace <> ":" <> name
    RawName text -> text

-- Tag names aren't *technically* namespaced but we will still compile them in a namespaced way by default
-- so we don't cause conflicts. This means that we need to use our own name mangling scheme instead of
-- <namespace>:<name> though! We use <namespace>__<name>
renderTagName :: (Name Resolved, TagProperties) -> Text
renderTagName (tagName, properties) = case tagName of
    NamespacedName namespace name
        | properties.isLiteral -> name
        | otherwise -> namespace <> "__" <> name
    RawName text -> text

renderObjectiveName :: (Name Resolved, ObjectiveProperties) -> Text
renderObjectiveName (tagName, properties) = case tagName of
    NamespacedName namespace name
        | properties.isLiteral -> name
        | otherwise -> namespace <> "__" <> name
    RawName text -> text

renderPlayerName :: PlayerName -> Text
renderPlayerName = \case
    QuotedPlayer text -> text
    PlayerName text -> text
