module Amethyst.Compile (runCompile) where

import Relude hiding (Ordering (..))
import Relude.Extra

import Amethyst.Syntax

import Data.Sequence ((<|))
import Data.Text qualified as Text
import Data.Unique
import System.FilePath ((</>))

-- TODO: use a proper text builder that isn't quadratic
data CompileEnv = MkCompileEnv
    { fileWriter :: FilePath -> Text -> IO ()
    , namespace :: Text
    , initializationFileContents :: IORef Text
    , stagedValues :: Map Text StagedValue
    }

newtype Compile a = MkCompile (ReaderT CompileEnv IO a)
    deriving newtype (Functor, Applicative, Monad)

stagedEnv :: Compile (Map Text StagedValue)
stagedEnv = MkCompile $ do
    env <- ask
    pure env.stagedValues

withStaged :: Text -> StagedValue -> Compile a -> Compile a
withStaged name value (MkCompile body) = MkCompile $ local (\env -> env{stagedValues = insert name value env.stagedValues}) body

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
                , stagedValues = mempty
                }
    let (MkCompile readerT) = compile program

    readerT `runReaderT` env
    finalInitFile <- readIORef initializationFileContents
    fileWriter ("data" </> toString program.namespace </> "function/init.mcfunction") finalInitFile
    fileWriter ("data/minecraft/tags/function/load.json") ("{\"values\":[\"" <> program.namespace <> ":init\"]}")

data StagedValue
    = StagedIntV Integer
    | StagedQuotedV Text
    deriving (Show)

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
        compileNewFunction (NamespacedName namespace name) commands
    DefineTag{} -> pure ()
    DefinePlayer{} -> pure ()
    DefineObjective{objectiveName} -> do
        emitInitCommand ("scoreboard objectives add " <> renderObjectiveName objectiveName <> " dummy")
    DefineSearchTree{name, rangeStart, rangeEnd, target, objective, varName, body} -> do
        start <- evalStagedInt rangeStart
        end <- evalStagedInt rangeEnd

        target <- compileScoreTarget target
        objective <- pure $ renderObjectiveName objective

        namespace <- currentNamespace

        let leafFunctionNameFor value = do
                (namespace <> ":generated/search_tree/" <> name <> "_" <> show value)

        let functionNameFor start end = do
                (namespace <> ":generated/search_tree/" <> name <> "_" <> show start <> "_" <> show end)

        -- generate leaf functions
        for_ @[] [start .. end] \i -> withStaged varName (StagedIntV i) do
            compileNewFunction (NamespacedName{namespace, name = "generated/search_tree/" <> name <> "_" <> show i}) body

        let go currentStart currentEnd = do
                let mid :: Integer = floor (fromInteger @Double (currentStart + currentEnd) / 2)

                let code =
                        Text.unlines
                            $ concat @[]
                                [
                                    [ "execute if score "
                                        <> target
                                        <> " "
                                        <> objective
                                        <> " matches "
                                        <> show mid
                                        <> " run return run function "
                                        <> leafFunctionNameFor mid
                                    ]
                                , [ "execute if score " <> target <> " " <> objective <> " matches " <> ".." <> show mid <> " run return run function " <> functionNameFor currentStart (mid - 1)
                                  | currentStart < mid
                                  ]
                                , [ "execute if score " <> target <> " " <> objective <> " matches " <> show mid <> ".." <> " run return run function " <> functionNameFor (mid + 1) currentEnd
                                  | currentEnd > mid
                                  ]
                                ]

                if (currentStart == start && currentEnd == end)
                    then compileNewRawFunction (NamespacedName{namespace, name}) code
                    else compileNewRawFunction (NamespacedName{namespace, name = "generated/search_tree/" <> name <> "_" <> show currentStart <> "_" <> show currentEnd}) code

                when (currentStart < mid) $ go currentStart (mid - 1)
                when (currentEnd > mid) $ go (mid + 1) currentEnd
        go start end

compileNewFunction :: Name Resolved -> Seq (Command Resolved) -> Compile ()
compileNewFunction name commands = do
    commandTexts <- traverse compileCommand commands
    compileNewRawFunction name (Text.intercalate "\n" (toList commandTexts))

compileNewRawFunction :: Name Resolved -> Text -> Compile ()
compileNewRawFunction name commands = do
    emitNamespacedFile ("function" </> toString name.name <> ".mcfunction") commands

compileCommand :: Command Resolved -> Compile Text
compileCommand = \case
    GenericCommand name arguments -> do
        argumentTexts <- traverse compileGenericArgument arguments
        pure (unwords (toList (name <| argumentTexts)))
    Function function -> ("function " <>) <$> compileFunction function
    TagAdd entity tagName -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " add " <> renderTagName tagName
    TagRemove entity tagName -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " remove " <> renderTagName tagName
    Say message -> do
        message <- compileStaged message
        pure $ "say " <> message
    ExecuteRun clauses command -> do
        clauses <- traverse compileExecuteClause clauses
        command <- compileCommand command
        pure $ "execute " <> Text.intercalate " " (toList (clauses <> ["run " <> command]))
    ExecuteIf clauses -> do
        clauses <- traverse compileExecuteClause clauses
        pure $ "execute " <> Text.intercalate " " (toList clauses)
    ReturnValue staged -> ("return " <>) <$> compileStaged staged
    ReturnRun command -> ("return run " <>) <$> compileCommand command
    ReturnFail -> pure "return fail"
    ScoreboardPlayersSet target objective value -> do
        target <- compileScoreTarget target
        objective <- pure $ renderObjectiveName objective
        value <- compileStaged value
        pure $ "scoreboard players set " <> target <> " " <> objective <> " " <> value
    ScoreboardPlayersAdd target objective value -> do
        target <- compileScoreTarget target
        objective <- pure $ renderObjectiveName objective
        value <- compileStaged value
        pure $ "scoreboard players add " <> target <> " " <> objective <> " " <> value

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
    IfEntity entity -> do
        entity <- compileEntity entity
        pure ("if entity " <> entity)
    IfFunction function -> ("if function " <>) <$> compileFunction function
    IfScoreMatches target objective range -> do
        target <- compileScoreTarget target
        range <- compileRange range
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

compileFunction :: Function Resolved -> Compile Text
compileFunction = \case
    FunctionName name -> pure $ renderName name
    FunctionLambda commands -> compileLambda commands

compileScoreTarget :: ScoreTarget Resolved -> Compile Text
compileScoreTarget = \case
    EntityScore entity -> compileEntity entity
    PlayerScore playerName -> pure (renderPlayerName playerName)

compileRange :: Range Resolved -> Compile Text
compileRange (MkRange start end) = do
    start <- compileStaged start
    end <- compileStaged end
    pure $ start <> ".." <> end

compileStaged :: Staged Resolved -> Compile Text
compileStaged expression = do
    value <- evalStaged expression
    compileStagedValue value

compileStagedValue :: StagedValue -> Compile Text
compileStagedValue = \case
    StagedIntV value -> pure $ show value
    StagedQuotedV value -> pure value

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
    compileNewFunction name commands
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
    TagSelector name -> pure $ "tag=" <> renderTagName name
    DistanceSelector range -> do
        range <- compileRange range
        pure $ "distance=" <> range
    GenericSelector selectorName argument -> do
        argument <- compileGenericArgument argument
        pure (selectorName <> "=" <> argument)

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

evalStaged :: Staged Resolved -> Compile StagedValue
evalStaged = \case
    StagedInt int -> pure (StagedIntV int)
    StagedVar name -> do
        env <- stagedEnv
        case lookup name env of
            Nothing -> error $ "staged variable not found at compile time: " <> show name
            Just value -> pure value
    StagedQuote value -> pure $ StagedQuotedV value

evalStagedInt :: (HasCallStack) => Staged Resolved -> Compile Integer
evalStagedInt staged =
    evalStaged staged >>= \case
        StagedIntV value -> pure value
        stagedValue@(StagedQuotedV _) -> error $ "staged expression should have evaluated to an int but was: " <> show stagedValue
