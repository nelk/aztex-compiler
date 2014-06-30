module Text.Aztex.Processing where

import Text.Aztex.Types

condense :: Aztex -> Aztex
condense (CommandBlock (TextBlock stuff)) = condense (TextBlock stuff)
condense (TextBlock (TextBlock stuff)) = condense (TextBlock stuff)
condense (MathBlock (TextBlock stuff)) = condense (TextBlock stuff)
condense (CommandBlock (MathBlock stuff)) = condense (MathBlock stuff)
condense (TextBlock (MathBlock stuff)) = condense (MathBlock stuff)
condense (MathBlock (MathBlock stuff)) = condense (MathBlock stuff)
condense (CommandBlock (CommandBlock stuff)) = condense (CommandBlock stuff)
condense (TextBlock (CommandBlock stuff)) = condense (CommandBlock stuff)
condense (MathBlock (CommandBlock stuff)) = condense (CommandBlock stuff)
condense (Block l) = Block $ map condense l
condense anythingElse = anythingElse
