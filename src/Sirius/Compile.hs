module Sirius.Compile (runCompile) where

import Relude

import Sirius.Syntax

import Control.Monad.Writer
import Data.Sequence ((<|))
import qualified Data.Text as Text
import Data.Unique

data CompileEnv = MkCompileEnv
    { fileWriter :: FilePath -> Text -> IO ()
    , namespace :: Text
    }

newtype Compile a = MkCompile (ReaderT CompileEnv IO a)
    deriving newtype (Functor, Applicative, Monad)

freshName :: Text -> Compile Name
freshName base = MkCompile do
    MkCompileEnv{namespace} <- ask

    unique <- liftIO newUnique
    pure (Namespaced{namespace, name = base <> "_" <> show (hashUnique unique)})

runCompile :: Program -> (FilePath -> Text -> IO ()) -> IO ()
runCompile program fileWriter = do
    let (MkCompile readerT) = compile program
    runReaderT readerT fileWriter

emitFile :: FilePath -> Text -> Compile ()
emitFile file contents = MkCompile do
    writer <- ask
    liftIO $ writer file contents

compile :: Program -> Compile ()
compile program = do
    traverse_ compileDeclaration program . declarations

compileDeclaration :: Declaration -> Compile ()
compileDeclaration = \case
    DefineFunction name commands -> compileFunction name commands

compileFunction :: Name -> Seq Command -> Compile ()
compileFunction name commands = do
    commandTexts <- traverse compileCommand commands
    emitFile (toString name <> ".mcfunction") (Text.intercalate "\n" (toList commandTexts))

compileCommand :: Command -> Compile Text
compileCommand = \case
    GenericCommand name arguments -> do
        argumentTexts <- traverse compileArgument arguments
        pure (unwords (toList (name <| argumentTexts)))
    TagAdd entity name -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " add " <> name
    TagRemove entity name -> do
        entityText <- compileEntity entity
        pure $ "tag " <> entityText <> " remove " <> name

compileArgument :: GenericArgument -> Compile Text
compileArgument = \case
    Literal text -> pure text
    Named name -> pure name
    Lambda commands -> do
        name <- freshName "generated/f"
        compileFunction name commands
        pure $ renderName name
    Int int -> pure (show int)

compileEntity :: Entity -> Compile Text
compileEntity = \case {}

renderName :: Name -> Text
renderName = \case
    NamespacedName namespace name -> namespace <> ":" <> name
    RawName text -> text
