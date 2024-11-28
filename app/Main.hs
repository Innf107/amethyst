module Main (main) where

import System.IO (hPutStrLn)
import Relude
import Amethyst.Compile qualified as Compile
import Amethyst.Parser qualified as Parser
import Amethyst.Resolve qualified as Resolve

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main =
    getArgs >>= \case
        [amethystFile] -> do
            contents :: Text <- decodeUtf8 <$> readFileLBS amethystFile

            parsedProgram <- case Parser.parse amethystFile contents of
                Left errorBundle -> do
                    hPutStrLn stderr (errorBundlePretty errorBundle)
                    exitFailure
                Right program -> pure program

            resolvedProgram <- Resolve.resolve parsedProgram >>= \case
                Left err -> error (show err)
                Right program -> pure program

            let writeOutput filePath contents = do
                    putStrLn ("Writing function: " <> filePath)

                    createDirectoryIfMissing True (takeDirectory filePath)
                    writeFileText filePath contents

            Compile.runCompile resolvedProgram writeOutput
        _ -> do
            hPutStrLn stderr "usage: amethyst <FILE>"
            exitFailure
