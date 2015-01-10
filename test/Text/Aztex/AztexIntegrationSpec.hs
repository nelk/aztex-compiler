{-#LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Text.Aztex.AztexIntegrationSpec where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Monad.RWS
import Data.Maybe
import System.IO
import Text.LaTeX hiding (between, TitlePage)

import Test.Hspec
import Text.RawString.QQ (r)

import Text.Aztex.Types
import Text.Aztex.Parser
import Text.Aztex.CodeGeneration
import Text.Aztex.Config

-- TODO: Clean up with EitherT IO and use Bifunctor map here.
parseText :: String -> String -> IO (Either AztexError Aztex)
parseText name text = do
  parseResults <- parseAztex name text
  case parseResults of
    Left e -> return $ Left  [show e]
    Right (aztex, _) -> return $ Right aztex


parseRenderText :: String -> String -> IO (Either AztexError Text.Text)
parseRenderText name text = do
  eitherAztex <- parseText name text
  case eitherAztex of
       Left e -> return $ Left e
       Right aztex -> do
        let (output, finalState, errors) = runRWS (generate aztex) AztexStyle builtInState
            title = fst $ fromJust $ titlePage finalState
            author = snd $ fromJust $ titlePage finalState
        if null errors
          then let renderedLatex = renderLatex output (titlePage finalState)
               in return $ Right renderedLatex
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
wrapLatexBoilerplate body = "\\documentclass[fleqn]{article}\\usepackage{amsmath}\\usepackage{graphicx}\\usepackage{enumerate}\\usepackage{braket}\\usepackage[height=9.00000in,width=6.50000in]{geometry}\\begin{document} " ++ body ++ "\\end{document}"

titlepageBoilerplate :: String -> String -> String
titlepageBoilerplate title author = "\\title{\\vspace*{\\fill}" ++ title ++ "}\\author{ " ++ author ++ "\\vspace*{\\fill}}"

genTest :: String -> String -> Expectation
genTest input output = do
  result <- parseRenderText "genTest" input
  case result of
    Right latex -> let latexString = Text.unpack latex
                   in case compareText latexString output of
                        Nothing -> return ()
                        Just (ia, ib) -> (drop ia latexString) `shouldBe` (drop ib output)
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

titlepageInput :: String
titlepageInput = [r|
$import aztex-lib/latex.azx
$titlepage @Title @{First Last}
|]

titlepageOutput :: String
titlepageOutput  = [r|
\begin{titlepage}
  \vfill{}
  \maketitle{}
  \thispagestyle{empty}
  \vfill{}
\end{titlepage}
|]


spec :: Spec
spec = describe "Aztex Parser" $ do
            it "correctly parses a simple file" $ genTest input1 (wrapLatexBoilerplate output1)
            it "creates title page" $ genTest titlepageInput (titlepageBoilerplate "Title" "First Last" ++ wrapLatexBoilerplate titlepageOutput)

main :: IO ()
main = hspec spec


