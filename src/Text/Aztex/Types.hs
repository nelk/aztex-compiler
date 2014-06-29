module Text.Aztex.Types where
  
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

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

