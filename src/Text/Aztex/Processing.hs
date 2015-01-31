{-#LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Text.Aztex.Processing where

import qualified Data.Map as Map
import Control.Monad.RWS
import Control.Applicative

import Text.Aztex.Helpers
import Text.Aztex.Types


-- Could use another data type?
expand :: Aztex -> RWS AztexStyle AztexError AztexState Aztex
expand Empty = return Empty
expand Whitespace = return Whitespace
expand EOL = return EOL

expand (CommandBlock aztex) = CommandBlock <$> expand aztex
expand (TextBlock aztex) = TextBlock <$> expand aztex
expand (MathBlock aztex) = MathBlock <$> expand aztex

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

expand (Import imp) = do
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

