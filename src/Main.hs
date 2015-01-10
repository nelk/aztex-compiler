module Main where

import Prelude
import qualified Data.Text.IO as Text
import System.Environment
import System.IO
import System.Exit
import Control.Monad.RWS
import Data.Maybe

import Text.Aztex.Types
import Text.Aztex.Parser
import Text.Aztex.Config
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
        Right (aztex, _) -> let (output, finalState, errors) = runRWS (generate aztex) AztexStyle builtInState
                                title = fst $ fromJust $ titlePage finalState
                                author = snd $ fromJust $ titlePage finalState
                       in do
                        hPutStrLn stderr $ "Generating " ++ fileName ++ "..."
                        if null errors
                          then let renderedLatex = renderLatex output (titlePage finalState >> return (title, author))
                               in Text.putStrLn renderedLatex >> hPutStrLn stderr "Success!"
                          else mapM_ (hPutStrLn stderr) errors >> exitFail






