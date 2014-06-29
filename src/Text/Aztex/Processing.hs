module Text.Aztex.Processing where

import Text.Aztex.Types

{-
spliceImports :: Aztex -> IO ([Aztex])
spliceImports (Import import_fname) = do
  parsedFile <- liftIO $ parseAztexFile import_fname
  case parsedFile of
    Left errors -> error $ "Parsing " ++ import_fname ++ " failed with errors: " ++ show errors
    Right importedAztex -> return $ aztexExtractFirstBlock importedAztex

spliceImports (CommandBlock a) = spliceImports a >>= return . aztexOneOrMakeBlock CommandBlock
spliceImports (TextBlock a) = spliceImports a >>= return . TextBlock
spliceImports (MathBlock a) = spliceImports a >>= return . MathBlock
spliceImports (Block blocks) = mapM spliceImports blocks
spliceImports allElse = return allElse
-}


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
