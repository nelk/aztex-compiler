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
import Control.Applicative
import Control.Monad.RWS
import Control.Monad.Identity
import Data.Maybe

import Text.Aztex.Types

renderLatex :: LaTeX -> Maybe (LaTeX, LaTeX) -> Text.Text
renderLatex t tpage = render $ wrapBody t tpage

-- Could use another data type?
generate :: Aztex -> RWS AztexStyle AztexError AztexState LaTeX

generate Empty = return mempty
generate Whitespace = return " "
generate EOL = return " " -- " ~\\newline "
generate (TitlePage _ _) = return mempty

generate (CommandBlock aztex) = generateWithModifiedModes (Just CommandMode) Nothing (generate aztex)

generate (TextBlock aztex) = do
  st <- get
  generateWithModifiedModes (Just TextMode) (Just LatexText) $
    if latexMode st == LatexMath
      then generate aztex >>= \t -> return (latexText t)
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
  result <- mconcat <$> mapM generate l
  newState <- get
  put $ newState { aztexMode = aztexMode saveState
                 , latexMode = latexMode saveState
                 }
  return result

generate (Import _) = tell ["InternalError: Tried to generate code for Import, which should have been expanded."] >> return mempty

generate (Token t) = return $ raw $ Text.pack t
generate (Quoted s) = return $ raw "``" <> raw (Text.pack s) <> raw "\""

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

generate (Superscript a) = do
  st <- get
  middle <- generate a
  return $ if latexMode st == LatexMath
    then raw "^{" <> middle <> raw "}"
    else raw "\\textsuperscript{" <> middle <> raw "}"

generate (Subscript a) = do
  st <- get
  middle <- generate a
  return $ if latexMode st == LatexMath
    then raw "_{" <> middle <> raw "}"
    else math $ raw "_{" <> latexText middle <> raw "}"

generate (ImplicitModeSwitch new_mode) = do
  st <- get
  put $ st { latexMode = new_mode }
  return mempty

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
  usepackage ["fleqn"] amsmath
  usepackage [] graphicx
  usepackage [] "amssymb"
  usepackage [] "enumerate"
  usepackage [] "braket"
  usepackage [] "listings"
  importGeometry [GHeight (In 9), GWidth (In 6.5)]

