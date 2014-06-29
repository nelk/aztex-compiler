module Text.Aztex.Parser where

import Control.Applicative
import Text.Aztex.Types
import Text.Aztex.Helpers
import Text.Aztex.Config
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Data.Map as Map


p_file :: AztexParser
p_file = p_whitespace *> ((eof *> return (Token "")) <|> (aztexOutmostBlock <$> p_blocks p_block p_whitespace)) <* eof

-- At least one block separated by whitespace.
p_blocks :: AztexParser -> AztexEmptyParser -> AztexParser
p_blocks block_parser separator = p_whitespace *> (Block <$> endBy block_parser separator)

p_whitespace :: AztexEmptyParser
p_whitespace = skipMany $ (oneOf " \v\f\t\r\n" >> return ()) <|> p_comment

p_spaces :: AztexEmptyParser
p_spaces = skipMany $ oneOf " \v\f\t"

p_comment :: AztexEmptyParser
p_comment = string aztexCommentStart *> skipMany (noneOf "\n\r") <* eol

-- TODO: Scope bindings during parsing as well.
-- Represents block in either text or math mode.
p_block :: AztexParser
p_block = p_typed_block
      <|> between (char '{') (char '}') (p_blocks p_block p_whitespace)
      <|> p_token

p_typed_block :: AztexParser
p_typed_block = (char '$' *> (CommandBlock <$> p_command_block))
            <|> (char '#' *> (MathBlock <$> p_block))
            <|> (char '@' *> (TextBlock <$> p_block))

p_token :: AztexParser
p_token = Token <$> many1 (noneOf $ " \n\r{}$#@" ++ aztexCommentStart)

p_command_block :: AztexParser
p_command_block = p_let_binding
              <|> p_def_binding
              <|> p_typed_block
              <|> between (char '{') (char '}') (p_blocks p_command_block p_whitespace)
              <|> p_command_call
              <|> p_import

p_exactly_n :: Int -> AztexEmptyParser -> AztexParser -> GenParser Char AztexState [Aztex]
p_exactly_n n sep parser = sequence $ replicate n (sep *> parser)

p_identifier :: GenParser Char AztexState String
p_identifier = do
  firstLetter <- letter
  rest <- many (letter <|> oneOf "123467890-_")
  return (firstLetter:rest)

p_let_binding :: AztexParser
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
p_def_binding :: AztexParser
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

p_import :: AztexParser
p_import = (do
    try (string "import")
    p_spaces
    import_prefix <- p_identifier
    p_spaces
    eol
    return (Import $ import_prefix ++ "." ++ aztexFileExtension)
  ) <?> "Incorrectly formatted import statement."

p_command_call :: AztexParser
p_command_call = (do
    name <- p_identifier
    s <- getState
    case Map.lookup name (bindings s) of
      Nothing -> fail $ "unknown identifier " ++ name
      Just (AztexFunction argNames _) -> do
        args <- p_exactly_n (length argNames) p_spaces p_block
        return $ CallBinding name args
  ) <?> "Incorrectly formatted function call."

eol :: AztexEmptyParser
eol = ( try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <|> (eof >> return "(eof)")
    ) >> return ()
