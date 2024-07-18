module Sirius.Compile (runCompile) where

import Relude

import Sirius.Syntax

import Data.Sequence ((<|))
import Data.Text qualified as Text
import Data.Unique

data CompileEnv = MkCompileEnv
    { fileWriter :: FilePath -> Text -> IO ()
    , namespace :: Text
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
    let env =
            MkCompileEnv
                { fileWriter
                , namespace = program.namespace
                }
    let (MkCompile readerT) = compile program
    readerT `runReaderT` env

emitFile :: FilePath -> Text -> Compile ()
emitFile file contents = MkCompile do
    MkCompileEnv{fileWriter} <- ask
    liftIO $ fileWriter file contents

compile :: Program Resolved -> Compile ()
compile program = do
    traverse_ compileDeclaration program.declarations

compileDeclaration :: Declaration Resolved -> Compile ()
compileDeclaration = \case
    DefineFunction name commands -> do
        namespace <- currentNamespace
        compileFunction (NamespacedName namespace name) commands
    DefineTag{} -> pure ()

compileFunction :: Name Resolved -> Seq (Command Resolved) -> Compile ()
compileFunction name commands = do
    commandTexts <- traverse compileCommand commands
    emitFile (toString name.name <> ".mcfunction") (Text.intercalate "\n" (toList commandTexts))

compileCommand :: Command Resolved -> Compile Text
compileCommand = \case
    GenericCommand name arguments -> do
        argumentTexts <- traverse compileGenericArgument arguments
        pure (unwords (toList (name <| argumentTexts)))
    Function name ->
        pure $ "function " <> renderName name
    TagAdd entity tagName -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " add " <> renderTagName tagName
    TagRemove entity tagName -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " remove " <> renderTagName tagName
    Say message -> pure $ "say " <> message

compileGenericArgument :: GenericArgument Resolved -> Compile Text
compileGenericArgument = \case
    GenericTag tagName -> pure (renderTagName tagName)
    Named name -> pure (renderName name)
    Literal text -> pure text
    Lambda commands -> do
        name <- freshName "generated/f"
        compileFunction name commands
        pure $ renderName name
    Int int -> pure (show int)
    GenericEntity entity -> compileEntity entity

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
