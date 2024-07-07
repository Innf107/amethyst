module Main (main) where

import Relude
import Data.Text.IO (hPutStrLn)
import qualified Sirius.Parser as Parser
import Sirius.Lexer qualified as Lexer
import qualified Sirius.Compile as Compile

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))

main :: IO ()
main = getArgs >>= \case
    [siriusFile] -> do
        contents <- decodeUtf8 <$> readFileLBS siriusFile
                
        tokens <- case Lexer.runLexer contents of
            Left err -> do
                hPutStrLn stderr ("LEXICAL ERROR: " <> show err)
                exitFailure
            Right tokens -> pure tokens

        program <- case Parser.runParseM (Parser.parse tokens) of
            Left err -> do
                hPutStrLn stderr "PARSE ERROR"
                exitFailure
            Right program -> pure program

        let writeOutput filePath contents = do
                let path = takeDirectory siriusFile </> "function" </> filePath

                putStrLn ("Writing function: " <> path)

                createDirectoryIfMissing True (takeDirectory path)
                writeFileText path contents

        Compile.runCompile program writeOutput
    _ -> do
        hPutStrLn stderr "usage: sirius <FILE>"
        exitFailure
