module Amethyst.Resolve (resolve) where

import Relude
import Relude.Extra

import Amethyst.Syntax
import Amethyst.Util (mapAccumLM)
import Control.Monad.Except (MonadError (throwError))
import Data.Set qualified as Set

data ResolutionError
    = UndefinedFunction Text
    | UndefinedTag Text
    | UndefinedPlayer Text
    | UndefinedObjective Text
    | UndefinedGeneric Text
    | NonSubtype StagedType StagedType
    deriving (Show)

data Env = MkEnv
    { namespace :: Text
    , tags :: Map Text (Name Resolved, TagProperties)
    , functions :: Map Text (Name Resolved)
    , players :: Set Text
    , objectives :: Map Text (Name Resolved, ObjectiveProperties)
    , staged :: Map Text StagedType
    }

-- TODO: accumulate more than one error if possible
newtype Resolve a = MkResolve (ExceptT ResolutionError IO a)
    deriving (Functor, Applicative, Monad, MonadError ResolutionError)

resolve :: Program Parsed -> IO (Either ResolutionError (Program Resolved))
resolve program = runResolve $ resolveProgram emptyEnv program
  where
    emptyEnv =
        MkEnv
            { namespace = program.namespace
            , tags = mempty
            , functions = mempty
            , players = mempty
            , objectives = mempty
            , staged = mempty
            }

runResolve :: Resolve a -> IO (Either ResolutionError a)
runResolve (MkResolve exceptT) =
    runExceptT exceptT

resolveProgram :: Env -> Program Parsed -> Resolve (Program Resolved)
resolveProgram env program = do
    (_env, declarations) <- mapAccumLM resolveDeclaration env program.declarations
    pure
        ( MkProgram
            { namespace = program.namespace
            , declarations
            }
        )

makeNamespaced :: Env -> Text -> Resolve (Name Resolved)
makeNamespaced env text = do
    pure (NamespacedName{namespace = env.namespace, name = text})

resolveDeclaration :: Env -> Declaration Parsed -> Resolve (Env, Declaration Resolved)
resolveDeclaration env = \case
    DefineFunction name commands -> do
        newName <- makeNamespaced env name
        let envWithFunction = env{functions = insert name newName env.functions}

        commands <- traverse (resolveCommand envWithFunction) commands

        pure (envWithFunction, DefineFunction name commands)
    DefineTag{tagName, literal} -> do
        newName <- makeNamespaced env tagName
        let properties =
                MkTagProperties
                    { isLiteral = literal
                    }
        let envWithTag = env{tags = insert tagName (newName, properties) env.tags}
        pure (envWithTag, DefineTag{tagName, literal})
    DefinePlayer player -> pure (env{players = Set.insert player env.players}, DefinePlayer player)
    DefineObjective{objectiveName, literal} -> do
        newName <- makeNamespaced env objectiveName
        let properties = MkObjectiveProperties{isLiteral = literal}
        let envWithObjective = env{objectives = insert objectiveName (newName, properties) env.objectives}
        pure (envWithObjective, DefineObjective{objectiveName = (newName, properties), literal})
    DefineSearchTree{name, rangeStart, rangeEnd, target, objective, varName, body} -> do
        rangeStart <- resolveStaged env IntT rangeStart
        rangeEnd <- resolveStaged env IntT rangeEnd
        target <- resolveScoreTarget env target
        objective <- resolveObjective env objective

        functionName <- makeNamespaced env name
        let envWithFunction = env{functions = insert name functionName env.functions}

        -- We do allow the search tree body to mention itself
        let innerEnv = envWithFunction{staged = insert varName IntT envWithFunction.staged}
        body <- traverse (resolveCommand innerEnv) body

        pure (envWithFunction, DefineSearchTree{..})

resolveCommand :: Env -> Command Parsed -> Resolve (Command Resolved)
resolveCommand env = \case
    GenericCommand command arguments -> do
        arguments <- traverse (resolveGenericArgument env) arguments
        pure (GenericCommand command arguments)
    Function function -> Function <$> resolveFunction env function
    TagAdd entity tagName -> do
        entity <- resolveEntity env entity
        tagName <- resolveTagName env tagName
        pure (TagAdd entity tagName)
    TagRemove entity tagName -> do
        entity <- resolveEntity env entity
        tagName <- resolveTagName env tagName
        pure (TagRemove entity tagName)
    Say message -> Say <$> resolveStaged env AnyT message
    ExecuteRun clauses command -> do
        clauses <- traverse (resolveExecuteClause env) clauses
        command <- resolveCommand env command
        pure (ExecuteRun clauses command)
    ExecuteIf clauses -> do
        clauses <- traverse (resolveExecuteClause env) clauses
        pure (ExecuteIf clauses)
    ReturnValue staged -> ReturnValue <$> resolveStaged env IntT staged
    ReturnFail -> pure ReturnFail
    ReturnRun command -> ReturnRun <$> resolveCommand env command
    ScoreboardPlayersGet target objective -> do
        target <- resolveScoreTarget env target
        objective <- resolveObjective env objective
        pure (ScoreboardPlayersGet target objective)
    ScoreboardPlayersSet target objective value -> do
        target <- resolveScoreTarget env target
        objective <- resolveObjective env objective
        value <- resolveStaged env IntT value
        pure (ScoreboardPlayersSet target objective value)
    ScoreboardPlayersAdd target objective value -> do
        target <- resolveScoreTarget env target
        objective <- resolveObjective env objective
        value <- resolveStaged env IntT value
        pure (ScoreboardPlayersAdd target objective value)
    ScoreboardPlayersOperation target1 objective1 operation target2 objective2 -> do
        target1 <- resolveScoreTarget env target1
        objective1 <- resolveObjective env objective1
        target2 <- resolveScoreTarget env target2
        objective2 <- resolveObjective env objective2
        pure (ScoreboardPlayersOperation target1 objective1 operation target2 objective2)

resolveExecuteClause :: Env -> ExecuteClause Parsed -> Resolve (ExecuteClause Resolved)
resolveExecuteClause env = \case
    QuotedClause text -> pure (QuotedClause text)
    Anchored anchorPoint -> pure $ Anchored anchorPoint
    As entity -> As <$> resolveEntity env entity
    At entity -> At <$> resolveEntity env entity
    Facing position -> pure $ Facing position
    FacingEntity entity anchorPoint -> do
        entity <- resolveEntity env entity
        pure (FacingEntity entity anchorPoint)
    In dimension -> pure (In dimension)
    PositionedAs entity -> PositionedAs <$> resolveEntity env entity
    RotatedAs entity -> RotatedAs <$> resolveEntity env entity
    Summon text -> pure (Summon text)
    If condition -> If <$> resolveIfCondition env condition
    Unless condition -> Unless <$> resolveIfCondition env condition
    Store value location -> Store value <$> resolveStoreLocation env location

resolveStoreLocation :: Env -> StoreLocation 'Parsed -> Resolve (StoreLocation 'Resolved)
resolveStoreLocation env = \case
    StoreScore target objective -> do
        target <- resolveScoreTarget env target
        objective <- resolveObjective env objective
        pure (StoreScore target objective)

resolveIfCondition :: Env -> IfCondition Parsed -> Resolve (IfCondition Resolved)
resolveIfCondition env = \case
    IfEntity entity -> do
        entity <- resolveEntity env entity
        pure (IfEntity entity)
    IfFunction functionName -> do
        functionName <- resolveFunction env functionName
        pure (IfFunction functionName)
    IfScoreMatches target objective range ->
        IfScoreMatches
            <$> resolveScoreTarget env target
            <*> resolveObjective env objective
            <*> resolveRange env range
    IfScore target1 objective1 comparison target2 objective2 -> do
        target1 <- resolveScoreTarget env target1
        objective1 <- resolveObjective env objective1
        target2 <- resolveScoreTarget env target2
        objective2 <- resolveObjective env objective2
        pure (IfScore target1 objective1 comparison target2 objective2)

resolveScoreTarget :: Env -> ScoreTarget Parsed -> Resolve (ScoreTarget Resolved)
resolveScoreTarget env = \case
    EntityScore entity -> EntityScore <$> resolveEntity env entity
    PlayerScore playerName -> PlayerScore <$> resolvePlayerName env playerName

resolveEntity :: Env -> Entity Parsed -> Resolve (Entity Resolved)
resolveEntity env = \case
    QuotedEntity quoted -> pure (QuotedEntity quoted)
    Selector targetSelector selectorArguments -> do
        targetSelectors <- traverse (resolveSelectorArgument env) selectorArguments
        pure (Selector targetSelector targetSelectors)

resolveSelectorArgument :: Env -> SelectorArgument Parsed -> Resolve (SelectorArgument Resolved)
resolveSelectorArgument env = \case
    GenericSelector name argument -> GenericSelector name <$> resolveGenericArgument env argument
    TagSelector name -> TagSelector <$> resolveTagName env name
    DistanceSelector range -> DistanceSelector <$> resolveRange env range

resolveGenericArgument :: Env -> GenericArgument Parsed -> Resolve (GenericArgument Resolved)
resolveGenericArgument env = \case
    Literal text -> pure $ Literal text
    Int int -> pure $ Int int
    Lambda commands -> do
        commands <- traverse (resolveCommand env) commands
        pure (Lambda commands)
    GenericEntity entity -> GenericEntity <$> resolveEntity env entity
    Named (RawName name) -> pure $ Literal name
    Named (NamespacedName{}) -> undefined
    Named (LocalName name) -> do
        case lookup name env.tags of
            Just tagName -> pure (GenericTag tagName)
            Nothing -> case lookup name env.functions of
                Just functionName -> pure (Named (functionName))
                Nothing -> throwError (UndefinedGeneric name)

resolveRange :: Env -> Range Parsed -> Resolve (Range Resolved)
resolveRange env (MkRange start end) =
    MkRange <$> resolveStaged env IntT start <*> resolveStaged env IntT end

resolveStaged :: Env -> StagedType -> Staged Parsed -> Resolve (Staged Resolved)
resolveStaged env expectedType = \case
    StagedInt int -> do
        assertSubtype IntT expectedType
        pure $ StagedInt int
    StagedVar name -> case lookup name env.staged of
        Nothing -> undefined
        Just actualType -> do
            assertSubtype actualType expectedType
            pure (StagedVar name)
    -- quoted values have type any
    StagedQuote quote -> pure (StagedQuote quote)

resolveFunction :: Env -> Function Parsed -> Resolve (Function Resolved)
resolveFunction env = \case
    FunctionName name -> FunctionName <$> resolveFunctionName env name
    FunctionLambda commands -> FunctionLambda <$> traverse (resolveCommand env) commands

resolveFunctionName :: Env -> Name Parsed -> Resolve (Name Resolved)
resolveFunctionName env = \case
    RawName raw -> pure $ RawName raw
    NamespacedName{} -> undefined
    LocalName name -> case lookup name env.functions of
        Nothing -> throwError (UndefinedFunction name)
        Just name -> pure name

resolveTagName :: Env -> TagName Parsed -> Resolve (TagName Resolved)
resolveTagName env = \case
    RawName raw -> pure $ (RawName raw, MkTagProperties{isLiteral = False})
    NamespacedName{} -> undefined
    LocalName name -> case lookup name env.tags of
        Nothing -> throwError (UndefinedTag name)
        Just name -> pure name

resolveObjective :: Env -> ObjectiveName Parsed -> Resolve (ObjectiveName Resolved)
resolveObjective env = \case
    RawName raw -> pure $ (RawName raw, MkObjectiveProperties{isLiteral = False})
    NamespacedName{} -> undefined
    LocalName name -> case lookup name env.objectives of
        Nothing -> throwError (UndefinedObjective name)
        Just name -> pure name

resolvePlayerName :: Env -> PlayerName -> Resolve PlayerName
resolvePlayerName env playerName = case playerName of
    QuotedPlayer{} -> pure playerName
    PlayerName playerName -> case lookup playerName env.players of
        Nothing -> throwError (UndefinedPlayer playerName)
        Just playerName -> pure (PlayerName playerName)

assertSubtype :: StagedType -> StagedType -> Resolve ()
assertSubtype type1 type2 = case (type1, type2) of
    (_, AnyT) -> pure ()
    (IntT, IntT) -> pure ()
    (AnyT, IntT) -> throwError (NonSubtype type1 type2)
