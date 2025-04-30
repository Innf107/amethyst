module Main (main) where

import Amethyst.Compile qualified as Compile
import Amethyst.Parser qualified as Parser
import Amethyst.Resolve qualified as Resolve
import Amethyst.Syntax
import Relude
import System.IO (hPutStrLn)

import Amethyst.Resolve (includeImportedEnv)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Megaparsec (errorBundlePretty)

data CompilationOptions = CompilationOptions
    { writeOutput :: FilePath -> Text -> IO ()
    }

-- TODO: treat relative files correctly
compileModuleAt :: CompilationOptions -> FilePath -> IO (Resolve.Env, Text)
compileModuleAt options filePath = do
    contents :: Text <- decodeUtf8 <$> readFileLBS filePath

    parsedProgram <- case Parser.parse filePath contents of
        Left errorBundle -> do
            hPutStrLn stderr (errorBundlePretty errorBundle)
            exitFailure
        Right program -> pure program

    environments <- traverse (\import_ -> compileModuleAt options (toString import_.targetFile)) parsedProgram.imports

    let recursivelyResolvedEnv =
            foldl'
                (\env (toBeIncluded, namespace) -> includeImportedEnv namespace toBeIncluded env)
                Resolve.emptyEnv
                environments

    (resolvedProgram, resolvedEnv) <-
        Resolve.resolve recursivelyResolvedEnv parsedProgram >>= \case
            Left err -> error (show err)
            Right program -> pure program
    -- TODO: might be a nice place for parallelism where we could spark off a self-contained compile job
    -- while the rest is still resolving?
    Compile.runCompile resolvedProgram options.writeOutput
    pure (resolvedEnv, resolvedProgram.namespace)

main :: IO ()
main =
    getArgs >>= \case
        [amethystFile] -> do
            let writeOutput filePath contents = do
                    putStrLn ("Writing function: " <> filePath)

                    createDirectoryIfMissing True (takeDirectory filePath)
                    writeFileText filePath contents
            let compilationOptions = CompilationOptions{writeOutput}
            _ <- compileModuleAt compilationOptions amethystFile
            pure ()
        _ -> do
            hPutStrLn stderr "usage: amethyst <FILE>"
            exitFailure
