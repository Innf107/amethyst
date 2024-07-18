module Sirius.Resolve (resolve) where

import Relude
import Relude.Extra

import Control.Monad.Except (MonadError (throwError))
import Sirius.Syntax
import Sirius.Util (mapAccumLM)

data ResolutionError
    = UndefinedFunction Text
    | UndefinedTag Text
    | UndefinedGeneric Text
    deriving (Show)

data Env = MkEnv
    { namespace :: Text
    , tags :: Map Text (Name Resolved, TagProperties)
    , functions :: Map Text (Name Resolved)
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

resolveCommand :: Env -> Command Parsed -> Resolve (Command Resolved)
resolveCommand env = \case
    GenericCommand command arguments -> do
        arguments <- traverse (resolveGenericArgument env) arguments
        pure (GenericCommand command arguments)
    Function name -> do
        functionName <- resolveFunction env name
        pure (Function functionName)
    TagAdd entity tagName -> do
        entity <- resolveEntity env entity
        tagName <- resolveTagName env tagName
        pure (TagAdd entity tagName)
    TagRemove entity tagName -> do
        entity <- resolveEntity env entity
        tagName <- resolveTagName env tagName
        pure (TagRemove entity tagName)
    Say message -> pure (Say message)

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

resolveFunction :: Env -> Name Parsed -> Resolve (Name Resolved)
resolveFunction env = \case
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