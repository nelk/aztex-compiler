{-#LANGUAGE OverloadedStrings #-}

module Text.Aztex.CodeGeneration where

import Text.LaTeX hiding (between)
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.Graphicx
import Text.LaTeX.Packages.Geometry
import Text.LaTeX.Base.Class (LaTeXC(..), comm1)

import qualified Data.Text as Text
import qualified Data.Map as Map
import Control.Monad.RWS
import Data.Maybe

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


generate :: Monad m => Aztex -> RWS AztexStyle AztexError AztexState (LaTeXT_ m)
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
  result <- foldl1 combine $ map generate l
  put saveState
  return $ raw "{" <> result <> raw "}"
  --return result
    where combine accum next_rws = do -- TODO: Use Transformer.
            previous <- accum
            next <- next_rws
            return $ previous <> " " <> next

generate (Import imports) = do
  st <- get
  put $ st{bindings = Map.union (bindings st) imports}
  return ""

generate (Token t) = return $ raw $ Text.pack t

generate (ImplicitModeSwitch new_mode) = do
  st <- get
  put $ st{latexMode = new_mode}
  return ""


renderLatex :: Monad m => LaTeXT_ m -> m Text.Text
renderLatex t = execLaTeXT (wrapBody t) >>= return . render


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
