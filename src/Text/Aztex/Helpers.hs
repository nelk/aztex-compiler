module Text.Aztex.Helpers where

import Text.Aztex.Types

aztexToFunction :: Aztex -> AztexFunction
aztexToFunction body = AztexFunction [] body

aztexDummyFunction :: AztexFunction
aztexDummyFunction = AztexFunction [] (Token "")

aztexExtractFirstBlock :: Aztex -> [Aztex]
aztexExtractFirstBlock (Block as) = as
aztexExtractFirstBlock (TextBlock a) = aztexExtractFirstBlock a
aztexExtractFirstBlock (MathBlock a) = aztexExtractFirstBlock a
aztexExtractFirstBlock (CommandBlock a) = aztexExtractFirstBlock a
aztexExtractFirstBlock _ = []

aztexOneOrMakeBlock :: (Aztex -> Aztex) -> [Aztex] -> Aztex
aztexOneOrMakeBlock ctor (single:[]) = ctor single
aztexOneOrMakeBlock ctor multiple = ctor $ Block multiple

