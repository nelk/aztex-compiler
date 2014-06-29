{-#LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import System.Environment
import System.IO
import Control.Applicative
import Control.Monad.RWS
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import qualified Data.Map as Map
import Text.LaTeX hiding (between)
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Base.Class (LaTeXC(..), liftL, liftL2, comm0, comm1)
--import Text.LaTeX.Base.Syntax (TeXArg(FixArg), LaTeX(TeXComm, TeXEnv))


data Aztex = CommandBlock Aztex
           | TextBlock Aztex
           | MathBlock Aztex
           | Token String 
           | Block [Aztex]
           | Binding String AztexFunction
           | CallBinding String [Aztex]
           | Import String
  deriving Show

data AztexFunction = AztexFunction [String] Aztex
  deriving Show

data AztexMode = CommandMode | TextMode | MathMode deriving (Show, Eq)
data LatexMode = LatexText | LatexMath deriving (Show, Eq)

data AztexState = AztexState { bindings :: Map.Map String AztexFunction
                             , aztexMode :: AztexMode
                             , latexMode :: LatexMode
                             }
  deriving Show

data AztexStyle = AztexStyle

type AztexError = [String]
type AztexParser = GenParser Char AztexState Aztex
type AztexEmptyParser = GenParser Char AztexState ()


aztexFileExtension :: String
aztexFileExtension = "azx"

aztexCommentStart :: String
aztexCommentStart = "%"

aztexToFunction :: Aztex -> AztexFunction
aztexToFunction body = AztexFunction [] body

aztexDummyFunction :: AztexFunction
aztexDummyFunction = AztexFunction [] (Token "")

builtInState :: AztexState
builtInState = AztexState {
    bindings = Map.fromList $
      [ --("cos", AztexFunction [] $ MathBlock (Token "\\cos"))
      ]
  , aztexMode = TextMode
  , latexMode = LatexText
  }

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
        Right aztex -> let (output, _, errors) = runRWS (generate aztex) AztexStyle builtInState
                       in do
                        hPutStrLn stderr $ "Generating " ++ fileName ++ "..."
                        --print aztex
                        if length errors == 0
                          then execLaTeXT (wrapBody output) >>= Text.putStr . render
                          else mapM_ (hPutStrLn stderr) errors


aztexOutmostBlock :: Aztex -> Aztex
aztexOutmostBlock = TextBlock

aztexExtractFirstBlock :: Aztex -> [Aztex]
aztexExtractFirstBlock (Block as) = as
aztexExtractFirstBlock (TextBlock a) = aztexExtractFirstBlock a
aztexExtractFirstBlock (MathBlock a) = aztexExtractFirstBlock a
aztexExtractFirstBlock (CommandBlock a) = aztexExtractFirstBlock a
aztexExtractFirstBlock _ = []


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


aztexOneOrMakeBlock :: (Aztex -> Aztex) -> [Aztex] -> Aztex
aztexOneOrMakeBlock ctor (single:[]) = ctor single
aztexOneOrMakeBlock ctor multiple = ctor $ Block multiple

{-
spliceImports :: Aztex -> IO ([Aztex])
spliceImports (Import import_fname) = do
  parsedFile <- liftIO $ parseAztexFile import_fname
  case parsedFile of
    Left errors -> error $ "Parsing " ++ import_fname ++ " failed with errors: " ++ show errors
    Right importedAztex -> return $ aztexExtractFirstBlock importedAztex

spliceImports (CommandBlock a) = spliceImports a >>= return . aztexOneOrMakeBlock CommandBlock
spliceImports (TextBlock a) = spliceImports a >>= return . TextBlock
spliceImports (MathBlock a) = spliceImports a >>= return . MathBlock
spliceImports (Block blocks) = mapM spliceImports blocks
spliceImports allElse = return allElse
-}


optimize :: Aztex -> Aztex
optimize (CommandBlock (TextBlock stuff)) = optimize (TextBlock stuff)
optimize (TextBlock (TextBlock stuff)) = optimize (TextBlock stuff)
optimize (MathBlock (TextBlock stuff)) = optimize (TextBlock stuff)
optimize (CommandBlock (MathBlock stuff)) = optimize (MathBlock stuff)
optimize (TextBlock (MathBlock stuff)) = optimize (MathBlock stuff)
optimize (MathBlock (MathBlock stuff)) = optimize (MathBlock stuff)
optimize (CommandBlock (CommandBlock stuff)) = optimize (CommandBlock stuff)
optimize (TextBlock (CommandBlock stuff)) = optimize (CommandBlock stuff)
optimize (MathBlock (CommandBlock stuff)) = optimize (CommandBlock stuff)
optimize (Block l) = Block $ map optimize l
optimize anythingElse = anythingElse


generate :: Monad m => Aztex -> RWS AztexStyle AztexError AztexState (LaTeXT_ m)
generate (CommandBlock aztex) = do
  st <- get
  put $ st{aztexMode = CommandMode}
  generate aztex

generate (TextBlock aztex) = do
  st <- get
  put $ st{aztexMode = TextMode, latexMode = LatexText}
  if latexMode st == LatexMath
    then generate aztex >>= return . latexText
    else generate aztex

generate (MathBlock aztex) = do
  st <- get
  put $ st{aztexMode = MathMode, latexMode = LatexMath}
  if latexMode st == LatexText
    then generate aztex >>= return . math
    else generate aztex

generate (Binding name fcn) = do
  st <- get
  put $ st{bindings = Map.insert name fcn (bindings st)}
  return ""

generate (CallBinding name args) = do
  st <- get
  case Map.lookup name (bindings st) of
    Nothing -> tell ["Identifier " ++ name ++ " used out of scope."] >> return ""
    Just (AztexFunction argNames fcnBody) -> do
      -- TODO: Union takes the left's value by implementation, but use something to guarantee this because it is crucial that it does this.
      -- Bind local arguments for this function's body.
      let bindingsWithLocal = Map.union (Map.fromList $ zip argNames $ map aztexToFunction args) (bindings st)
      put $ st{bindings = bindingsWithLocal}
      fcnResult <- generate fcnBody
      put st
      return fcnResult

generate (Block l) = do
  saveState <- get
  result <- foldl1 combine $ map generate l
  put saveState
  return $ raw "{" <> result <> raw "}"
    where combine accum next_rws = do -- TODO: Use Transformer.
            previous <- accum
            next <- next_rws
            return $ previous <> " " <> next

generate (Token t) = return $ raw $ Text.pack t


latexText :: LaTeXC l => l -> l
latexText = comm1 "text"

wrapBody :: Monad m => LaTeXT_ m -> LaTeXT_ m
wrapBody theBody = do
  --let vspace_star_fill = raw "\\vspace*{\\fill}"
  --title $ vspace_star_fill >> theTitle
  --author $ theAuthor >> vspace_star_fill
  thePreamble
  document $ do
    --theTitlePage
    theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
  documentclass [Fleqn] article
  usepackage [] amsmath
  usepackage [] graphicx
  --usepackage [] "braket"
  importGeometry [GHeight (In 9), GWidth (In 6.5)]



