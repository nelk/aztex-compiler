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
import Control.Monad.Identity
import Data.Maybe
import Control.Applicative

import Text.Aztex.Helpers
import Text.Aztex.Types

generateWithModifiedModes :: Maybe AztexMode
                          -> Maybe LatexMode
                          -> RWS AztexStyle AztexError AztexState LaTeX
                          -> RWS AztexStyle AztexError AztexState LaTeX
generateWithModifiedModes amode_m lmode_m gen = do
  st <- get
  put $ st{ aztexMode = fromMaybe (aztexMode st) amode_m
          , latexMode = fromMaybe (latexMode st) lmode_m
          }
  result <- gen
  st' <- get
  put $ st'{ aztexMode = aztexMode $ if isJust amode_m then st else st'
           , latexMode = latexMode $ if isJust lmode_m then st else st'
           }
  return result

-- Could use another data type?
generate :: Aztex -> RWS AztexStyle AztexError AztexState LaTeX

generate Empty = return mempty

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
      then liftM math $ generate aztex
      else generate aztex

generate (Binding name fcn) = do
  st <- get
  put $ st{bindings = Map.insert name fcn (bindings st)}
  return mempty

generate (CallBinding name _) = tell ["InternalError: Tried to generate code for CallBinding of \"" ++ name ++ "\", which should have been expanded."] >> return mempty

generate (Block l) = do
  saveState <- get
  (_, result) <- foldl combine (return (False, "")) l
  newState <- get
  put $ newState { aztexMode = aztexMode saveState
                 , latexMode = latexMode saveState
                 }
  return result
    where combine :: RWS AztexStyle AztexError AztexState (Bool, LaTeX)
                  -> Aztex
                  -> RWS AztexStyle AztexError AztexState (Bool, LaTeX)
          combine accum next_aztex = do -- TODO: Use Transformer.
            (use_whitespace, previous) <- accum
            next <- generate next_aztex
            case next_aztex of
              Token t | t == "{" || t == "}" -> return (False, previous <> next)
              CommandBlock (CallBinding n _) | n == "lbrace" || n == "rbrace" -> return (False, previous <> next)
              _ | use_whitespace -> return (True, previous <> " " <> next)
              _ -> return (True, previous <> next)

generate (Import _) = tell ["InternalError: Tried to generate code for Import, which should have been expanded."] >> return mempty

generate (Token t) = return $ raw $ Text.pack t

generate (Parens a) = do
  st <- get
  middle <- generate a
  return $ if latexMode st == LatexMath
    then raw "\\left(" <> middle <> raw "\\right)"
    else raw "(" <> middle <> raw ")"

generate (Brackets a) = do
  st <- get
  middle <- generate a
  return $ if latexMode st == LatexMath
    then raw "\\left[" <> middle <> raw "\\right]"
    else raw "[" <> middle <> raw "]"

generate (ImplicitModeSwitch new_mode) = do
  st <- get
  put $ st { latexMode = new_mode }
  return mempty

renderLatex :: LaTeX -> Maybe (LaTeX, LaTeX) -> Text.Text
renderLatex t tpage = render $ wrapBody t tpage

latexText :: LaTeXC l => l -> l
latexText = comm1 "text"

wrapBody :: LaTeX -> Maybe (LaTeX, LaTeX) -> LaTeX
wrapBody theBody tpage =
  let vspace_star_fill :: LaTeX = raw "\\vspace*{\\fill}"
      (setTitle, setAuthor, addTitlePage) = case tpage of
        Nothing -> (mempty, mempty, mempty)
        Just (theTitle, theAuthor) ->
          ( title $ vspace_star_fill <> theTitle
          , author $ theAuthor <> vspace_star_fill
          , theTitlePage
          )
  in setTitle <>
     setAuthor <>
     thePreamble <>
     document (addTitlePage <> theBody)

theTitlePage :: LaTeX
theTitlePage = tpageEnv $
  vfill <>
  maketitle <>
  thispagestyle "empty" <>
  vfill

tpageEnv :: LaTeXC l => l -> l
tpageEnv = liftL $ TeXEnv "titlepage" []

thePreamble :: LaTeX
thePreamble = runIdentity $ execLaTeXT $ do
  documentclass [Fleqn] article
  usepackage [] amsmath
  usepackage [] graphicx
  usepackage [] "enumerate"
  usepackage [] "braket"
  importGeometry [GHeight (In 9), GWidth (In 6.5)]

