module Main where

import Prelude
import qualified Data.Text.IO as Text
import System.Environment
import System.IO
import Control.Monad.RWS

import Text.Aztex.Types
import Text.Aztex.Parser
import Text.Aztex.Config
import Text.Aztex.CodeGeneration


usage :: IO ()
usage = putStrLn $ "./aztex file" ++ "." ++ aztexFileExtension

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (fileName:_) -> do
      parseResults <- parseAztexFile fileName
      case parseResults of
        Left e -> hPrint stderr e
        Right (aztex, _) -> let (output, _, errors) = runRWS (generate aztex) AztexStyle builtInState
                       in do
                        hPutStrLn stderr $ "Generating " ++ fileName ++ "..."
                        --print aztex
                        if length errors == 0
                          then renderLatex output >>= Text.putStrLn >> hPutStrLn stderr "Success!"
                          else mapM_ (hPutStrLn stderr) errors >> hPutStrLn stderr "Failure!"






