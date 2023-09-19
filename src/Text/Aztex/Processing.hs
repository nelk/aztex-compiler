{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.Aztex.Processing where

import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Applicative
import Data.List (intercalate)

import Text.Aztex.Helpers
import Text.Aztex.Types


-- Could use another data type?
expand :: Aztex -> RWS AztexStyle AztexError AztexState Aztex
expand Empty = return Empty
expand Whitespace = return Whitespace
expand EOL = return EOL
expand (Comment _) = return Empty

expand (CommandBlock aztex) = CommandBlock <$> expand aztex
expand (TextBlock aztex) = TextBlock <$> expand aztex
expand (MathBlock aztex) = MathBlock <$> expand aztex

--TODO: Split usage of Token between actual input tokens and raw text that has been generated during processing.
expand (Verbatim (CallBinding name [])) = do
  -- Lookup the call directly and then use what's in it verbatim.
  -- Some hacks required to get this working.
  st <- get
  case Map.lookup name (bindings st) of
    Nothing -> tell ["Identifier " ++ name ++ " used out of scope in verbatim call."] >> return Empty
    Just (AztexFunction [] (Block bodies)) -> return $ Token $ concatMap verbatim bodies
    Just (AztexFunction [] (CommandBlock (Block bodies))) -> return $ Token $ concatMap verbatim bodies
    Just (AztexFunction [] body) -> return $ Token $ verbatim body
    Just (AztexFunction _ _) -> tell ["Malformed verbatim body!"] >> return Empty
expand (Verbatim _) = undefined

expand (Binding name fcn) = do
  st <- get
  put $ st{bindings = Map.insert name fcn (bindings st)}
  return Empty

expand (CallBinding name args) = do
  st <- get
  case Map.lookup name (bindings st) of
    Nothing -> tell ["Identifier " ++ name ++ " used out of scope."] >> return Empty
    Just (AztexFunction argNames fcnBody) -> do
      -- Evaluate all arguments.
      expandedArgs :: [Aztex] <- mapM expand args
      -- Bind local arguments for this function's body.
      -- Note: Map.union is left-biased.
      let bindingsWithLocal = Map.union (Map.fromList $ zip argNames $ map aztexToFunction expandedArgs) (bindings st)
          {-
      let bindingsWithLocal = Map.union (Map.fromList $ uncurry makeBinding <$> zip argNames args) (bindings st)
          makeBinding bindingName arg = case Map.lookup bindingName (bindings st) of
                                          Nothing ->  (bindingName, aztexToFunction arg)
                                          Just alreadyBound -> (bindingName, alreadyBound)
                                          -}
      put $ st{bindings = bindingsWithLocal}
      fcnResult <- expand fcnBody
      -- Pop bindings.
      st' <- get
      put st'{bindings = bindings st}
      return fcnResult

expand (Block l) = Block <$> mapM expand l

expand (Import _ imp) = do
  st <- get
  put $ st {bindings = Map.union (bindings st) imp}
  return Empty

expand t@(Token _) = return t
expand (Parens a) = Parens <$> expand a
expand (Brackets a) = Brackets <$> expand a
expand (Subscript a) = Subscript <$> expand a
expand (Superscript a) = Superscript <$> expand a
expand (Quoted a) = return $ Quoted a
expand a@(ImplicitModeSwitch _) = return a

expand (TitlePage title_a author_a) = do
  st <- get
  title_l <- expand title_a
  author_l <- expand author_a
  put st {titlePage = Just (title_l, author_l)}
  return Empty

verbatim :: Aztex -> String
verbatim Empty = ""
verbatim Whitespace = " "
verbatim EOL = "\n"
verbatim (Comment c) = "%" ++ c

verbatim (CommandBlock aztex) = "$" ++ verbatim aztex
verbatim (TextBlock aztex) = "@" ++ verbatim aztex
verbatim (MathBlock aztex) = "#" ++ verbatim aztex
verbatim (Verbatim aztex) = verbatim aztex

verbatim (Binding name (AztexFunction args body)) =
  let defOrLet | null args = "let"
               | otherwise = "def"
      argString | null args = ""
                | otherwise = "(" ++ intercalate ", " args ++ ")"
  in defOrLet ++ " " ++ name ++ " " ++ argString ++ " = " ++ verbatim body

verbatim (Import name _) = "import " ++ name

verbatim (CallBinding name args) = name ++ concatMap verbatim args
verbatim (Token t) = t
verbatim (Block parts) = "{" ++ concatMap verbatim parts ++ "}"
-- Hack.
verbatim (Parens (Block bs)) = "(" ++ concatMap verbatim bs ++ ")"
verbatim (Parens a) = "(" ++ verbatim a ++ ")"
-- Hack.
verbatim (Brackets (Block bs)) = "[" ++ concatMap verbatim bs ++ "]"
verbatim (Brackets a) = "[" ++ verbatim a ++ "]"
verbatim (Subscript a) = "_" ++ verbatim a
verbatim (Superscript a) = "^" ++ verbatim a
verbatim (Quoted a) = "\"" ++ a ++ "\""
verbatim (ImplicitModeSwitch _) = "" -- TODO.
verbatim (TitlePage title_a author_a) = "" -- TODO.



