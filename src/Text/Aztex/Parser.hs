module Text.Aztex.Parser where

import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec hiding (many, optional, try, (<|>))
import Text.Parsec.Prim (ParsecT, runParserT, try, putState)

import Text.Aztex.Types
import Text.Aztex.Helpers
import Text.Aztex.Config

type AztexParser = ParsecT String AztexState IO

-- TODO: BUG - using align* doesn't work correctly with newlines "\\" because they can't be nested in brace blocks.

parseAztex_ :: String -> String -> Map.Map String AztexBindings -> IO (Either ParseError AztexParseResult)
parseAztex_ name text alreadyParsed = do
  either_aztex <- runParserT parseFile (builtInState{imports = alreadyParsed}) name text
  case either_aztex of
    Left e -> return $ Left e
    Right aztex_results -> return $ Right aztex_results

parseAztexFile_ :: String -> Map.Map String AztexBindings -> IO (Either ParseError AztexParseResult)
parseAztexFile_ fileName alreadyParsed = do
  file <- readFile fileName
  hPutStrLn stderr $ "Parsing " ++ fileName ++ "..."
  parseAztex_ fileName file alreadyParsed

parseAztex :: String -> String -> IO (Either ParseError AztexParseResult)
parseAztex name text = parseAztex_ name text Map.empty

parseAztexFile :: String -> IO (Either ParseError AztexParseResult)
parseAztexFile fileName = parseAztexFile_ fileName Map.empty

parseFile :: AztexParser AztexParseResult
parseFile = do
  _ <- parseWhitespace
  all_aztex <- (eof *> return (Token "")) <|> (aztexOutmostBlock <$> parseBlocks parseBlock parseWhitespace)
  eof
  final_state <- getState
  return (all_aztex, exports final_state)

-- At least one block separated and started by seperator, which is also put into the block. Discards Empty values.
parseBlocks :: AztexParser Aztex -> AztexParser Aztex -> AztexParser Aztex
parseBlocks block_parser separator = do
  firstW <- separator
  blocks <- many separatedBlocks
  return $ Block $ filter (/= Empty) (firstW:join blocks)
   where separatedBlocks = do
          b <- block_parser
          w <- separator
          return [b, w]

parseWhitespace :: AztexParser Aztex
parseWhitespace = do
  w <- many $ (oneOf " \v\f\t" >> return Whitespace) <|> (oneOf "\r\n" >> return EOL) <|> (parseComment >> return Empty)
  return $ if EOL `elem` w
              then EOL
              else if Whitespace `elem` w
                      then Whitespace
                      else Empty

parseSpaces :: AztexParser Aztex
parseSpaces = do
  sps <- many $ oneOf " \v\f\t"
  return $ if not (null sps) then Whitespace else Empty

parseComment :: AztexParser Aztex
parseComment = do
  _ <- string aztexCommentStart
  c <- many (noneOf "\n\r")
  _ <- parseEOL
  return $ Comment c

-- Represents block in either text or math mode.
parseBlock :: AztexParser Aztex
parseBlock = parseTypedBlock
      <|> between (char '{') (char '}') (parseBlocks parseBlock parseWhitespace)
      <|> between (char '(') (char ')') (Parens <$> parseBlocks parseBlock parseWhitespace)
      <|> between (char '[') (char ']') (Brackets <$> parseBlocks parseBlock parseWhitespace)
      <|> (Quoted <$> (char '"' *> many (noneOf "\"") <* char '"'))
      <|> (Superscript <$> (char '^' *> parseBlock))
      <|> (Subscript <$> (char '_' *> parseBlock))
      <|> parseToken

parseTypedBlock :: AztexParser Aztex
parseTypedBlock = (char '$' *> (CommandBlock <$> parseCommandBlock))
            <|> (char '#' *> (MathBlock <$> parseBlock))
            <|> (char '@' *> (TextBlock <$> parseBlock))

parseToken :: AztexParser Aztex
parseToken = Token <$> many1 (noneOf $ " \n\r{}()[]$#@_^" ++ aztexCommentStart)

parseCommandBlock :: AztexParser Aztex
parseCommandBlock = parseLetBinding
              <|> parseDefBinding
              <|> parseTypedBlock
              <|> between (char '{') (char '}') (parseBlocks parseCommandBlock parseWhitespace)
              <|> parseImport
              <|> parseExport
              <|> parseCommandCall

parseExactlyN :: Int -> AztexParser Aztex -> AztexParser Aztex -> AztexParser [Aztex]
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
    _ <- parseSpaces
    name <- parseIdentifier
    _ <- parseSpaces
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
    _ <- parseSpaces
    name <- parseIdentifier
    _ <- parseSpaces
    argNames <- between (char '(') (char ')') $ sepBy (parseSpaces *> many (noneOf " ,()") <* parseSpaces) (char ',')
    _ <- parseSpaces
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

fileNameFromPath :: String -> String
fileNameFromPath s = reverse $ f_ s ""
  where f_ "" out = out
        f_ (a:as) out
          | a `elem` "/\\" = f_ as ""
          | otherwise      = f_ as (a:out)

parseImport :: AztexParser Aztex
parseImport = (do
    _ <- try (string "import")
    _ <- parseSpaces
    import_fpath <- parseFilepath
    _ <- parseSpaces
    _ <- parseEOL
    st <- getState
    -- TODO: Keep track of base directory of file being parsed so that library files don't need to include "aztex-lib/" when they import.
    let import_fname = import_fpath -- fileNameFromPath import_fpath
    case Map.lookup import_fname (imports st) of
      Just ims -> do
        putState st{bindings = Map.union (bindings st) ims}
        return $ Import import_fname ims
      Nothing -> do
        either_parsed_import <- liftIO $ parseAztexFile_ import_fpath $ imports st
        case either_parsed_import of
          Left errors -> error $ "Parsing " ++ import_fpath ++ " failed with errors: " ++ show errors
          Right (_, ims) -> do
            updateState $ \s -> s
              { bindings = Map.union (bindings s) ims
              , imports = Map.insert import_fname ims (imports s)
              }
            return $ Import import_fname ims

  ) <?> "Incorrectly formatted import statement."

parseExport :: AztexParser Aztex
parseExport = do
  _ <- try (string "export")
  _ <- parseSpaces
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

parseEOL :: AztexParser Aztex
parseEOL = ( try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> (eof >> return "(eof)")
      ) >> return EOL

