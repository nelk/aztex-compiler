Aztex Compiler
==============
This compiler is for the Aztex language - front-end typesetting language by S. Menakuru and A. Klen.
This compiler is currently a prototype and under heavy development - but give it a whirl!

Usage
-----
./releases/aztex-dev sample_scripts/test.azx > test.tex && pdflatex test.tex && gnome-open test.pdf

Intro
-----
LaTeX produces great results, but can be tedious to write by hand (Knuth never intended for anyone to write it by hand!).
This gives us two options - use a WYSIWYG editor, or use another front-end language for it.
Aztex is such a language.

It was designed to be less noisy than LaTeX and have the following features:
  - scoped variable and function bindings
  - light-weight switching between text/math that just works
  - sane defaults
  - separate content from presentation (much more than latex does)

Example
-------
First, an example to get us started:
In LaTeX:
```
\newcommand\Prob{\text{Prob}}
\begin{enumerate}
  \item Observe the following about $a$:
    \begin{equation}
      \text{Let } a \text{ represent } \Prob_{+x} = \frac{1}{2} \cos{\pi t}
    \end{equation}
\end{enumerate}
```

In Aztex:
```
let Prob = @Prob
enumerate${
  item@{
    Observe the following about #a:
    $equation{
      @Let a @represent $Prob _{+x} = $frac 1 2 $cos $pi t
    }
  }
}
```
Language
---------
The idea is that there are 3 modes that you can easily enter, exit, and nest:
  - Command Mode ($)
  - Text Mode (@)
  - Math Mode (#)
The latter two represent modes in latex, while Command Mode is something specifically
added by Aztex.
While in Text Mode, most characters are taken verbatim and inserted into the document.
While in Math Mode, most characters will be rendered as math by LaTeX.
Command mode will lets you bind variables and functions and evaluate them (later on it might compute math for you as well).

You can easily bind a variable like this:
  let a = #{1 + 1 = 2}
Now you can use "$a" in text or math mode, or just "a" in command mode to insert this math-mode expression anywhere you want.
You can also bind a function like this:
  def surround(left, right, middle) = ${left middle right}
As you can see, the arguments are just bound identifiers in the body.
You can call this function like "$surround ( ) abc", or "$surround ( ) {hello there}".
Definitions are only available after they are defined and until the end of its parent block.

The $#@ operators operate on the next block and functions will take the next n blocks as arguments.
A "block" is either a bunch of non-reserved characters until the next space, or everything between curly braces {}.

You can import a file using the "import" keyword in command mode to bring in all of its exported names (useful for libraries) into the calling scope.
You can export a binding by using "export" before its definition. Exports work anywhere in the document.

-- Formal specification in the works, but this should get you going --

Current Bugs
------------
- Putting explicit new lines "\\" in math environments breaks because LaTeX requires brace blocks to end before the end of a line in these environments.
- Too many nested blocks of braces in generated LaTeX (not a huge issue, but will be solved).
- Imports aren't cached.

Upcoming Features
-----------------
- "splice" - like import, but also inlines file's LaTeX.
- More command-mode features - evaluating basic math expressions, loops?


