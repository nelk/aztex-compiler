{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Text.Aztex.CodeGeneration where

import Text.LaTeX hiding (between, TitlePage)
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Base.Class (LaTeXC(..), comm1, liftL)
import Text.LaTeX.Base.Syntax (LaTeX(TeXEnv))

import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Monad.Identity (Identity(..))
import Data.Maybe
import Control.Monad

import Text.Aztex.Helpers
import Text.Aztex.Types


generateWithModifiedModes :: Maybe AztexMode
                          -> Maybe LatexMode
                          -> RWS AztexStyle AztexError AztexState (LaTeXT_ m)
                          -> RWS AztexStyle AztexError AztexState (LaTeXT_ m)
generateWithModifiedModes amode_m lmode_m gen = do
  st <- get
  put $ st{ aztexMode = fromMaybe (aztexMode st) amode_m
          , latexMode = fromMaybe (latexMode st) lmode_m
          }
  result <- gen
  st' <- get
  put $ st'{ aztexMode = if isJust amode_m then aztexMode st else aztexMode st'
           , latexMode = if isJust lmode_m then latexMode st else latexMode st'
           }
  return result


generate :: forall m. (Monad m) => Aztex -> RWS AztexStyle AztexError AztexState (LaTeXT_ m)
generate (CommandBlock aztex) = generateWithModifiedModes (Just CommandMode) Nothing (generate aztex)

generate (TextBlock aztex) = do
  st <- get
  generateWithModifiedModes (Just TextMode) (Just LatexText) $
    if latexMode st == LatexMath
      then generate aztex >>= \t -> return (latexText $ " " <> t <> " ")
      else generate aztex

generate (MathBlock aztex) = do
  st <- get
  generateWithModifiedModes (Just MathMode) (Just LatexMath) $
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
      -- Restore bindings to before call.
      st' <- get
      put st'{bindings = bindings st}
      return fcnResult

generate (Block l) = do
  saveState <- get
  (_, result) <- foldl combine (return (True, "")) l
  put saveState
  return result
    where combine :: Monad m
                  => RWS AztexStyle AztexError AztexState (Bool, LaTeXT_ m)
                  -> Aztex
                  -> RWS AztexStyle AztexError AztexState (Bool, LaTeXT_ m)
          combine accum next_aztex = do -- TODO: Use Transformer.
            (use_whitespace, previous) <- accum
            next <- generate next_aztex
            case next_aztex of
              Token t | t == "{" || t == "}" -> return (False, previous <> next)
              CommandBlock (CallBinding n _) | n == "lbrace" || n == "rbrace" -> return (False, previous <> next)
              _ | use_whitespace -> return (True, previous <> " " <> next)
              _ -> return (True, previous <> next)

generate (Import imports) = do
  st <- get
  put $ st{bindings = Map.union (bindings st) imports}
  return ""

generate (Token t) = return $ raw $ Text.pack t

generate (Parens a) = do
  st <- get
  middle <- generate a
  if latexMode st == LatexMath
    then return $ raw "\\left(" <> middle <> raw "\\right)"
    else return $ raw "(" <> middle <> raw ")"

generate (Brackets a) = do
  st <- get
  middle <- generate a
  if latexMode st == LatexMath
    then return $ raw "\\left[" <> middle <> raw "\\right]"
    else return $ raw "[" <> middle <> raw "]"

generate (ImplicitModeSwitch new_mode) = do
  st <- get
  put $ st{latexMode = new_mode}
  return ""

generate (TitlePage title_a author_a) = do
  st <- get
  --title <- generate title_a
  --author <- generate author_a
  put $ st{titlePage = Just (title_a, author_a)}
  return ""


renderLatex :: Monad m => LaTeXT_ m -> Maybe (LaTeXT_ m, LaTeXT_ m) -> m Text.Text
renderLatex t tpage = execLaTeXT (wrapBody t tpage) >>= return . render


latexText :: LaTeXC l => l -> l
latexText = comm1 "text"

wrapBody :: Monad m => LaTeXT_ m -> Maybe (LaTeXT_ m, LaTeXT_ m) -> LaTeXT_ m
wrapBody theBody tpage = do
  let vspace_star_fill = raw "\\vspace*{\\fill}"
  case tpage of
    Nothing -> return ()
    Just (theTitle, theAuthor) -> do
      title $ vspace_star_fill <> theTitle
      author $ theAuthor <> vspace_star_fill
  thePreamble
  document $ do
    if isJust tpage then theTitlePage else return ()
    theBody

theTitlePage :: Monad m => LaTeXT_ m
theTitlePage = tpageEnv $ do
  vfill
  maketitle
  thispagestyle "empty"
  vfill

tpageEnv :: LaTeXC l => l -> l
tpageEnv = liftL $ TeXEnv "titlepage" []

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
  documentclass [Fleqn] article
  usepackage [] amsmath
  usepackage [] graphicx
  usepackage [] "braket"
  importGeometry [GHeight (In 9), GWidth (In 6.5)]

