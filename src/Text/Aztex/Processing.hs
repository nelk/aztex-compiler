module Text.Aztex.Processing where

import Text.Aztex.Types

optimize :: Aztex -> Aztex
optimize (CommandBlock (TextBlock stuff)) = optimize (TextBlock stuff)
optimize (TextBlock (TextBlock stuff)) = optimize (TextBlock stuff)
optimize (MathBlock (TextBlock stuff)) = optimize (TextBlock stuff)
optimize (CommandBlock (MathBlock stuff)) = optimize (MathBlock stuff)
optimize (TextBlock (MathBlock stuff)) = optimize (MathBlock stuff)
optimize (MathBlock (MathBlock stuff)) = optimize (MathBlock stuff)
optimize (CommandBlock (CommandBlock stuff)) = optimize (CommandBlock stuff)
optimize (TextBlock (CommandBlock stuff)) = optimize (CommandBlock stuff)
optimize (MathBlock (CommandBlock stuff)) = optimize (CommandBlock stuff)
optimize (Block l) = Block $ map optimize l
optimize anythingElse = anythingElse
