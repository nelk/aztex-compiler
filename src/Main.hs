module Main where

import Prelude
import qualified Data.Text.IO as Text
import System.Environment
import System.IO
import Control.Monad.RWS

import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import Text.Aztex.Types
import Text.Aztex.Config
import Text.Aztex.Parser
import Text.Aztex.Processing
import Text.Aztex.CodeGeneration


usage :: IO ()
usage = putStrLn $ "./aztex file" ++ "." ++ aztexFileExtension

parseAztexFile :: String -> IO (Either ParseError Aztex)
parseAztexFile fileName = do
  file <- readFile fileName
  hPutStrLn stderr $ "Parsing " ++ fileName ++ "..."
  return $ runParser p_file builtInState fileName file

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage
    (fileName:_) -> do
      parseResults <- parseAztexFile fileName
      case parseResults of
        Left e -> hPrint stderr e
        Right aztex -> let optimizedAztex = optimize aztex
                           (output, _, errors) = runRWS (generate optimizedAztex) AztexStyle builtInState
                       in do
                        hPutStrLn stderr $ "Generating " ++ fileName ++ "..."
                        --print aztex
                        if length errors == 0
                          then renderLatex output >>= Text.putStrLn
                          else mapM_ (hPutStrLn stderr) errors






