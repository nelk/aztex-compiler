module Text.Aztex.Types where

import qualified Data.Map as Map

-- TODO: Make math mode notation better, eg. turn operators like ">=" into what "$ge" does now.

data Aztex = CommandBlock Aztex
           | TextBlock Aztex
           | MathBlock Aztex
           | Token String
           | Quoted String
           | Parens Aztex
           | Brackets Aztex
           | Block [Aztex]
           | Binding String AztexFunction
           | CallBinding String [Aztex]
           | Import AztexBindings
           | ImplicitModeSwitch LatexMode
           | TitlePage Aztex Aztex
           | Subscript Aztex
           | Superscript Aztex
           | Whitespace
           | EOL
           | Empty
  deriving (Show, Eq)

data AztexFunction = AztexFunction [String] Aztex
  deriving (Show, Eq)


data AztexMode = CommandMode | TextMode | MathMode deriving (Show, Eq)
data LatexMode = LatexText | LatexMath deriving (Show, Eq)

type AztexBindings = Map.Map String AztexFunction

data AztexState = AztexState { bindings :: AztexBindings
                             , exports :: AztexBindings
                             , imports :: Map.Map String AztexBindings
                             , aztexMode :: AztexMode
                             , latexMode :: LatexMode
                             , titlePage :: Maybe (Aztex, Aztex)
                             }
  deriving Show

data AztexStyle = AztexStyle

type AztexError = [String]
type AztexParseResult = (Aztex, AztexBindings) -- AST and exports

