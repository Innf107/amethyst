module Main (main) where

import System.IO (hPutStrLn)
import Relude
import Sirius.Compile qualified as Compile
import Sirius.Parser qualified as Parser
import Sirius.Resolve qualified as Resolve

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main =
    getArgs >>= \case
        [siriusFile] -> do
            contents :: Text <- decodeUtf8 <$> readFileLBS siriusFile

            parsedProgram <- case Parser.parse siriusFile contents of
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
            hPutStrLn stderr "usage: sirius <FILE>"
            exitFailure
