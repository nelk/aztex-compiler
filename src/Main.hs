module Main where

import Prelude
import qualified Data.Text.IO as Text
import System.Environment
import System.IO
import System.Exit
import Control.Monad.RWS
import Control.Arrow (first)
import Data.Maybe

import Text.Aztex.Types
import Text.Aztex.Parser
import Text.Aztex.Config
import Text.Aztex.Processing
import Text.Aztex.CodeGeneration


usage :: IO ()
usage = putStrLn $ "./aztex file" ++ "." ++ aztexFileExtension

exitFail :: IO ()
exitFail = hPutStrLn stderr "Failure!" >> exitWith (ExitFailure 1)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (fileName:_) -> do
      parseResults <- parseAztexFile fileName
      case parseResults of
        Left e -> hPrint stderr e >> exitFail
        Right (aztex, _) -> let (output, finalState, errors) = runRWS (expand aztex >>= generate) AztexStyle builtInState
                                (mTitle, titleErrors) = maybe (Nothing, mempty) (\(t, _) -> first Just $ evalRWS (generate t) AztexStyle builtInState) $ titlePage finalState
                                (mAuthor, authorErrors) = maybe (Nothing, mempty) (\(_, a) -> first Just $ evalRWS (generate a) AztexStyle builtInState) $ titlePage finalState
                                allErrors = titleErrors <> authorErrors <> errors
                       in do
                        hPutStrLn stderr $ "Generating " ++ fileName ++ "..."
                        -- hPutStrLn stderr $ "Nonexpanded:" ++ show aztex ++ "\n\n\n\n"
                        -- hPutStrLn stderr $ "Expanded:" ++ show output
                        if null allErrors
                          then let renderedLatex = renderLatex output $ do
                                      title <- mTitle
                                      author <- mAuthor
                                      return (title, author)
                               in Text.putStrLn renderedLatex >> hPutStrLn stderr "Success!"
                          else mapM_ (hPutStrLn stderr) errors >> exitFail






