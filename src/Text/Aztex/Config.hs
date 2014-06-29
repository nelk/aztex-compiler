module Text.Aztex.Config where

import Text.Aztex.Types
import qualified Data.Map as Map

aztexFileExtension :: String
aztexFileExtension = "azx"

aztexCommentStart :: String
aztexCommentStart = "%"

builtInState :: AztexState
builtInState = AztexState {
    bindings = Map.fromList $
      [ ("lbrace", AztexFunction [] $ Token "\\{")
      , ("rbrace", AztexFunction [] $ Token "\\}")
      , ("leftBrace", AztexFunction [] $ MathBlock (Token "\\left\\{"))
      , ("rightBrace", AztexFunction [] $ MathBlock (Token "\\right\\}"))
      ]
  , aztexMode = TextMode
  , latexMode = LatexText
  }


aztexOutmostBlock :: Aztex -> Aztex
aztexOutmostBlock = TextBlock
