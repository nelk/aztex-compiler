module Text.Aztex.Parser where

import System.IO
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec hiding (many, optional, try, (<|>))
import Text.Parsec.Prim (ParsecT, runParserT, try)

import Text.Aztex.Types
import Text.Aztex.Helpers
import Text.Aztex.Config
import Text.Aztex.Processing

type AztexParser = ParsecT String AztexState IO

-- TODO: BUG - calling a function in a function where they share an argument with the same name will cause infinite recursion!
-- TODO: BUG - using align* doesn't work correctly with newlines "\\" because they can't be nested in brace blocks.

parseAztexFile :: String -> IO (Either ParseError AztexParseResult)
parseAztexFile fileName = do
  file <- readFile fileName
  hPutStrLn stderr $ "Parsing " ++ fileName ++ "..."
  either_aztex <- runParserT p_file builtInState fileName file
  case either_aztex of
    Left e -> return $ Left e
    Right aztex_results -> return $ Right (condense $ fst aztex_results, snd aztex_results)


p_file :: AztexParser AztexParseResult
p_file = do
  p_whitespace
  all_aztex <- (eof *> return (Token "")) <|> (aztexOutmostBlock <$> p_blocks p_block p_whitespace)
  eof
  final_state <- getState
  return $ (all_aztex, exports final_state)

-- At least one block separated by whitespace.
p_blocks :: AztexParser Aztex -> AztexParser () -> AztexParser Aztex
p_blocks block_parser separator = p_whitespace *> (Block <$> endBy block_parser separator)

p_whitespace :: AztexParser ()
p_whitespace = skipMany $ (oneOf " \v\f\t\r\n" >> return ()) <|> p_comment

p_spaces :: AztexParser ()
p_spaces = skipMany $ oneOf " \v\f\t"

p_comment :: AztexParser ()
p_comment = string aztexCommentStart *> skipMany (noneOf "\n\r") <* p_eol

-- TODO: Scope bindings during parsing!
-- Represents block in either text or math mode.
p_block :: AztexParser Aztex
p_block = p_typed_block
      <|> between (char '{') (char '}') (p_blocks p_block p_whitespace)
      <|> p_token

p_typed_block :: AztexParser Aztex
p_typed_block = (char '$' *> (CommandBlock <$> p_command_block))
            <|> (char '#' *> (MathBlock <$> p_block))
            <|> (char '@' *> (TextBlock <$> p_block))

p_token :: AztexParser Aztex
p_token = Token <$> many1 (noneOf $ " \n\r{}$#@" ++ aztexCommentStart)

p_command_block :: AztexParser Aztex
p_command_block = p_let_binding
              <|> p_def_binding
              <|> p_typed_block
              <|> between (char '{') (char '}') (p_blocks p_command_block p_whitespace)
              <|> p_import
              <|> p_export
              <|> p_command_call

p_exactly_n :: Int -> AztexParser () -> AztexParser Aztex -> AztexParser [Aztex]
p_exactly_n n sep parser = sequence $ replicate n (sep *> parser)

p_identifier :: AztexParser String
p_identifier = do
  firstLetter <- letter
  rest <- many (letter <|> oneOf "123467890-_")
  return (firstLetter:rest)

p_filepath :: AztexParser String
p_filepath = many1 (noneOf " \n\r")

p_let_binding :: AztexParser Aztex
p_let_binding = (do
    try (string "let")
    p_spaces
    name <- p_identifier
    p_spaces
    char '='
    bound <- p_blocks p_block p_spaces
    let bound_fcn = AztexFunction [] bound
    updateState (\s -> s{bindings = Map.insert name bound_fcn (bindings s)})
    return $ Binding name $ bound_fcn
  ) <?> "Incorrectly formatted let binding."

-- TODO: Higher order functions
p_def_binding :: AztexParser Aztex
p_def_binding = (do
    try (string "def")
    p_spaces
    name <- p_identifier
    p_spaces
    argNames <- between (char '(') (char ')') $ sepBy (p_spaces *> (many $ noneOf " ,()") <* p_spaces) (char ',')
    p_spaces
    char '='
    -- Bind local names.
    st <- getState
    let bindingsWithLocal = Map.union (bindings st) (Map.fromList $ zip argNames $ repeat aztexDummyFunction)
    setState $ st{bindings = bindingsWithLocal}
    bound <- p_blocks p_block p_spaces
    let bound_fcn = AztexFunction argNames bound
    updateState (\s -> s{bindings = Map.insert name bound_fcn (bindings s)})
    return $ Binding name $ bound_fcn
  ) <?> "Incorrectly formatted function definition."

p_import :: AztexParser Aztex
p_import = (do
    try (string "import")
    p_spaces
    import_fname <- p_filepath
    p_spaces
    p_eol
    --let import_fname = import_prefix ++ "." ++ aztexFileExtension
    either_parsed_import <- liftIO $ parseAztexFile import_fname
    case either_parsed_import of
      Left errors -> error $ "Parsing " ++ import_fname ++ " failed with errors: " ++ show errors
      Right (_, imports) -> do
        updateState $ \s -> s{bindings = Map.union (bindings s) imports}
        return $ Import imports

  ) <?> "Incorrectly formatted import statement."

p_export :: AztexParser Aztex
p_export = do
  try (string "export")
  p_spaces
  exported_binding@(Binding export_name export_fcn) <- p_let_binding <|> p_def_binding <?> "Can only export let or def expressions."
  updateState $ \s -> s{exports = Map.insert export_name export_fcn $ exports s}
  return exported_binding


p_command_call :: AztexParser Aztex
p_command_call = (do
    name <- p_identifier
    s <- getState
    case Map.lookup name (bindings s) of
      Nothing -> fail $ "unknown identifier " ++ name
      Just (AztexFunction argNames _) -> do
        args <- p_exactly_n (length argNames) p_spaces p_block
        return $ CallBinding name args
  ) <?> "Incorrectly formatted function call."

p_eol :: AztexParser ()
p_eol = ( try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> (eof >> return "(eof)")
      ) >> return ()

