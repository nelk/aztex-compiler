module Text.Aztex.Config where

import Text.Aztex.Types
import qualified Data.Map as Map

aztexFileExtension :: String
aztexFileExtension = "azx"

aztexCommentStart :: String
aztexCommentStart = "%"

builtInState :: AztexState
builtInState = AztexState {
    bindings = Map.fromList
      [ ("lbrace", AztexFunction [] $ Token "{")
      , ("rbrace", AztexFunction [] $ Token "}")
      , ("lparen", AztexFunction [] $ Token "(")
      , ("rparen", AztexFunction [] $ Token ")")
      , ("leftBrace", AztexFunction [] $ MathBlock (Token "\\left\\{"))
      , ("rightBrace", AztexFunction [] $ MathBlock (Token "\\right\\}"))
      , ("implicit_math", AztexFunction [] $ ImplicitModeSwitch LatexMath)
      , ("implicit_text", AztexFunction [] $ ImplicitModeSwitch LatexText)
      , ("titlepage", AztexFunction ["title", "author"] $ TitlePage (CallBinding "title" []) (CallBinding "author" []))
      , ("literalnewline", AztexFunction [] $ Token "\n")
      ]
  , exports = Map.empty
  , imports = Map.empty
  , aztexMode = TextMode
  , latexMode = LatexText
  , titlePage = Nothing
  }


aztexOutmostBlock :: Aztex -> Aztex
aztexOutmostBlock = TextBlock

