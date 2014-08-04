{-#LANGUAGE QuasiQuotes #-}

module Text.Aztex.AztexIntegrationSpec where

import qualified Data.Text as Text
import Control.Monad.RWS
import Data.Maybe

import Test.Hspec
import Text.RawString.QQ (r)

import Text.Aztex.Types
import Text.Aztex.Parser
import Text.Aztex.CodeGeneration
import Text.Aztex.Config


parseText :: String -> String -> IO (Either AztexError String)
parseText name text = do
  parseResults <- parseAztex name text
  case parseResults of
    Left e -> return $ Left $ [show e]
    Right (aztex, _) -> let (output, finalState, errors) = runRWS (generate aztex) AztexStyle builtInState
                            (title, _, _) = runRWS (generate $ fst $ fromJust $ titlePage finalState) AztexStyle builtInState
                            (author, _, _) = runRWS (generate $ snd $ fromJust $ titlePage finalState) AztexStyle builtInState
                   in do
                    if length errors == 0
                      then renderLatex output (titlePage finalState >> return (title, author)) >>= return . Right . Text.unpack
                      else return $ Left errors


skippableWhitespace :: String
skippableWhitespace = " \n"

compareText :: String -> String -> Maybe (Int, Int)
compareText a b = compareText_ a b 0 0

compareText_ :: String -> String -> Int -> Int -> Maybe (Int, Int)
compareText_ [] [] _ _ = Nothing
compareText_ (a:as) (b:bs) ia ib | a == b = compareText_ as bs (ia + 1) (ib + 1)
compareText_ (w:as) bs ia ib | w `elem` skippableWhitespace = compareText_ as bs (ia + 1) ib
compareText_ as (w:bs) ia ib | w `elem` skippableWhitespace = compareText_ as bs ia (ib + 1)
compareText_ _ _ ia ib = Just (ia, ib)


wrapLatexBoilerplate :: String -> String
wrapLatexBoilerplate body = "\\documentclass[fleqn]{article}\\usepackage{amsmath}\\usepackage{graphicx}\\usepackage{braket}\\usepackage[height=9.00000in,width=6.50000in]{geometry}\\begin{document} " ++ body ++ "\\end{document}"

simpleGenTest :: Expectation
simpleGenTest = do
  result <- parseText "simpleGenTest" input1
  let expected = wrapLatexBoilerplate output1
  case result of
    Right latex -> case compareText latex expected of
                    Nothing -> return ()
                    Just (ia, ib) -> (drop ia latex) `shouldBe` (drop ib expected)
    Left errors -> fail $ unlines errors

input1 :: String
input1 = [r|
$import aztex-lib/latex.azx
$import aztex-lib/amsmath.azx

@{
  $enumerate{
    $item{
      My first point % (not shown)
    }

    $item{
      My second point is made of subpoints:
      $itemize{
        $item {A sub-point}
        $item {Another sub-point}
      }
    }

    $item{
      My third point
      $item{Guess this works too...}
    }

    $item{
      #{$frac 1 2}
      @{Let #a be my variable.  }
      $math{
        a^2 + b^2 &= $braces {$frac{$cos(a+b)} {2$pi}} \\
        a + 1 &= 1 + a
      }
    }
  }

  %$environment @enumerate { $item hi }
}
|]

output1 :: String
output1 = [r|
\begin{enumerate}
  \item My first point
  \item My second point is made of subpoints:
    \begin{itemize}
      \item A sub-point
      \item Another sub-point
    \end{itemize}

  \item My third point
  \item Guess this works too... 

  \item
    $\frac{1}{2}$
    Let $a$ be my variable.
    \begin{align*}
      a^2 + b^2 &= \left\{ \frac{\cos\left(a+b\right)}{2\pi} \right\} \\
      a + 1 &= 1 + a
    \end{align*}
\end{enumerate}
|]

spec :: Spec
spec = describe "Aztex Parser" $ do
            it "correctly parses simple file" simpleGenTest

main :: IO ()
main = hspec spec


