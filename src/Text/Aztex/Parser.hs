module Text.Aztex.Parser where

import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec hiding (many, optional, try, (<|>))
import Text.Parsec.Prim (ParsecT, runParserT, try)

import Text.Aztex.Types
import Text.Aztex.Helpers
import Text.Aztex.Config

type AztexParser = ParsecT String AztexState IO

-- TODO: BUG - using align* doesn't work correctly with newlines "\\" because they can't be nested in brace blocks.

parseAztex :: String -> String -> IO (Either ParseError AztexParseResult)
parseAztex name text = do
  either_aztex <- runParserT parseFile builtInState name text
  case either_aztex of
    Left e -> return $ Left e
    Right aztex_results -> return $ Right aztex_results

parseAztexFile :: String -> IO (Either ParseError AztexParseResult)
parseAztexFile fileName = do
  file <- readFile fileName
  hPutStrLn stderr $ "Parsing " ++ fileName ++ "..."
  parseAztex fileName file

parseFile :: AztexParser AztexParseResult
parseFile = do
  parseWhitespace
  all_aztex <- (eof *> return (Token "")) <|> (aztexOutmostBlock <$> parseBlocks parseBlock parseWhitespace)
  eof
  final_state <- getState
  return (all_aztex, exports final_state)

-- At least one block separated by whitespace.
parseBlocks :: AztexParser Aztex -> AztexParser () -> AztexParser Aztex
parseBlocks block_parser separator = parseWhitespace *> (Block <$> endBy block_parser separator)

parseWhitespace :: AztexParser ()
parseWhitespace = skipMany $ void (oneOf " \v\f\t\r\n") <|> parseComment

parseSpaces :: AztexParser ()
parseSpaces = skipMany $ oneOf " \v\f\t"

parseComment :: AztexParser ()
parseComment = string aztexCommentStart *> skipMany (noneOf "\n\r") <* parseEOL

-- TODO: Scope bindings during parsing!
-- Represents block in either text or math mode.
parseBlock :: AztexParser Aztex
parseBlock = parseTypedBlock
      <|> between (char '{') (char '}') (parseBlocks parseBlock parseWhitespace)
      <|> between (char '(') (char ')') (Parens <$> parseBlocks parseBlock parseWhitespace)
      <|> between (char '[') (char ']') (Brackets <$> parseBlocks parseBlock parseWhitespace)
      <|> parseToken

parseTypedBlock :: AztexParser Aztex
parseTypedBlock = (char '$' *> (CommandBlock <$> parseCommandBlock))
            <|> (char '#' *> (MathBlock <$> parseBlock))
            <|> (char '@' *> (TextBlock <$> parseBlock))

parseToken :: AztexParser Aztex
parseToken = Token <$> many1 (noneOf $ " \n\r{}()[]$#@" ++ aztexCommentStart)

parseCommandBlock :: AztexParser Aztex
parseCommandBlock = parseLetBinding
              <|> parseDefBinding
              <|> parseTypedBlock
              <|> between (char '{') (char '}') (parseBlocks parseCommandBlock parseWhitespace)
              <|> parseImport
              <|> parseExport
              <|> parseCommandCall

parseExactlyN :: Int -> AztexParser () -> AztexParser Aztex -> AztexParser [Aztex]
parseExactlyN n sep parser = replicateM n (sep *> parser)

parseIdentifier :: AztexParser String
parseIdentifier = do
  firstLetter <- letter
  rest <- many (letter <|> oneOf "123467890-_")
  return (firstLetter:rest)

parseFilepath :: AztexParser String
parseFilepath = many1 (noneOf " \n\r")

parseLetBinding :: AztexParser Aztex
parseLetBinding = (do
    _ <- try (string "let")
    parseSpaces
    name <- parseIdentifier
    parseSpaces
    _ <- char '='
    bound <- parseBlocks parseBlock parseSpaces
    let bound_fcn = AztexFunction [] bound
    updateState (\s -> s{bindings = Map.insert name bound_fcn (bindings s)})
    return $ Binding name bound_fcn
  ) <?> "Incorrectly formatted let binding."

-- TODO: Higher order functions
parseDefBinding :: AztexParser Aztex
parseDefBinding = (do
    _ <- try (string "def")
    parseSpaces
    name <- parseIdentifier
    parseSpaces
    argNames <- between (char '(') (char ')') $ sepBy (parseSpaces *> many (noneOf " ,()") <* parseSpaces) (char ',')
    parseSpaces
    _ <- char '='
    -- Bind local names.
    st <- getState
    let bindingsWithLocal = Map.union (bindings st) (Map.fromList $ zip argNames $ repeat aztexDummyFunction)
    setState $ st{bindings = bindingsWithLocal}
    bound <- parseBlocks parseBlock parseSpaces
    let bound_fcn = AztexFunction argNames bound
    updateState (\s -> s{bindings = Map.insert name bound_fcn (bindings s)})
    return $ Binding name bound_fcn
  ) <?> "Incorrectly formatted function definition."

parseImport :: AztexParser Aztex
parseImport = (do
    _ <- try (string "import")
    parseSpaces
    import_fname <- parseFilepath
    parseSpaces
    parseEOL
    --let import_fname = import_prefix ++ "." ++ aztexFileExtension
    either_parsed_import <- liftIO $ parseAztexFile import_fname
    case either_parsed_import of
      Left errors -> error $ "Parsing " ++ import_fname ++ " failed with errors: " ++ show errors
      Right (_, imports) -> do
        updateState $ \s -> s{bindings = Map.union (bindings s) imports}
        return $ Import imports

  ) <?> "Incorrectly formatted import statement."

parseExport :: AztexParser Aztex
parseExport = do
  _ <- try (string "export")
  parseSpaces
  exported_binding@(Binding export_name export_fcn) <- parseLetBinding <|> parseDefBinding <?> "Can only export let or def expressions."
  updateState $ \s -> s{exports = Map.insert export_name export_fcn $ exports s}
  return exported_binding


parseCommandCall :: AztexParser Aztex
parseCommandCall = (do
    name <- parseIdentifier
    s <- getState
    case Map.lookup name (bindings s) of
      Nothing -> fail $ "unknown identifier " ++ name
      Just (AztexFunction argNames _) -> do
        args <- parseExactlyN (length argNames) parseSpaces parseBlock
        return $ CallBinding name args
  ) <?> "Incorrectly formatted function call."

parseEOL :: AztexParser ()
parseEOL = ( try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> (eof >> return "(eof)")
      ) >> return ()

