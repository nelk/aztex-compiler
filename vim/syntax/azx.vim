" Vim syntax file
" Language:             Aztex
" Maintainer:           Nelk <nelkishere@gmail.org>
" Original Author: Alex Klen
" Last Change: 14 Jan 2015
" Syntax matched to Aztex Release: 0.1
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

" Use case sensitive matching of keywords
syn case match

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Syntax group definitions
syn keyword aztexKeyword  let def import export

syn keyword aztexPredef lbrace rbrace lparen rparen leftBrace rightBrace titlepage
syn keyword aztexLatex latexfn1 latexfn2 environment environment_opt enumerate enumerateN itemize item itemN section sectionN sectionAlph newline nl textbf bf bold textit it italic underline quote
syn keyword aztexAmsmath align align_star equation math cos sin tan cot frac hat int integral partial sum rightarrow leftarrow exp expfrac difr part hbar braces abs le ge in max Max alpha Alpha beta Beta gamma Gamma delta Delta epsilon Epsilon zeta Zeta eta Eta theta Theta iota Iota kappa Kappa lambda Lambda mu Mu nu Nu xi Xi omicron Omicron pi Pi rho Rho sigma Sigma tau Tau upsilon Upsilon phi Phi chi Chi psi Psi omega Omega

" Add functions defined in .m file being read to list of highlighted functions
function! s:CheckForFunctions()
  let i = 1
  while i <= line('$')
    let line = getline(i)
    " Only look for functions at start of line.
    " Commented function, '% function', will not trigger as match returns 3
    if match(line, '(def|let) *\w+') >= 0
      let line = substitute(line, 'def *(\w+)\(', '\1', '')
      let line = substitute(line, 'let *(\w+) *=', '\1', '')
      if !empty(line)
        execute "syn keyword aztexFunction" nfun
      endif
    endif
    let i = i + 1
  endwhile
endfunction

call s:CheckForFunctions()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Define clusters for ease of writing subsequent rules
"syn cluster AllFuncVarCmd contains=aztexVariable,aztexFunction,aztexCommand
"syn cluster AllFuncSetCmd contains=aztexSetVarFun,aztexFunction,aztexCommand
syn cluster aztexFuncVars contains=aztexKeywords,aztexPredef,aztexLatex,aztexFunction

" Switch highlighting of variables based on coding use.
" Query -> Constant, Set -> Function
" order of items is is important here
"syn match aztexQueryVar "\<\h\w*[^(]"me=e-1  contains=@AllFuncVarCmd
"syn match aztexSetVar   "\<\h\w*\s*("me=e-1  contains=@AllFuncSetCmd
"syn match aztexQueryVar "\<\h\w*\s*\((\s*)\)\@="  contains=@AllFuncVarCmd

" Don't highlight aztex keywords on LHS of '=', these are user vars
"syn match aztexUserVar  "\<\h\w*\ze[^<>!~="']\{-}==\@!"
"syn match aztexUserVar  "\<\h\w*\s*[<>!~=]=" contains=aztexVariable

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Errors (placed early so they may be overriden by more specific rules
" Struct with nonvalid identifier starting with number (Example: 1.a or a.1b)
"syn region aztexError  start="\<\d\+\(\w*\.\)\@="  end="[^0-9]"he=s-1 oneline
"syn region aztexError  start="\.\d\+\(\w*\)\@="hs=s+1  end="[^0-9]"he=s-1 oneline
" Numbers with double decimal points (Example: 1.2.3)
"syn region aztexError  start="\<-\?\d\+\.\d\+\.[^*/\\^]"hs=e-1 end="\>"  oneline
"syn region aztexError  start="\<-\?\d\+\.\d\+[eEdD][-+]\?\d\+\.[^*/\\^]"hs=e-1 end="\>"  oneline

" Operators
" Uncommment "Hilink aztexOperator" below to highlight these
syn match aztexLogicalOperator     "[&|~!]"
syn match aztexArithmeticOperator  "[-+*/^]"
syn match aztexRelationalOperator  "[=!~]=\?"
syn match aztexRelationalOperator  "[<>]=\?"
syn cluster aztexOperator contains=aztexLogicalOperator,aztexArithmeticOperator,aztexRelationalOperator

" User Variables
" Uncomment this syntax group and "Hilink aztexIdentifier" below to highlight
"syn match aztexIdentifier  "\<\h\w*\>"

" Environments. Note - last has highest priority.
" Note: Keepend not working so using negative lookahead
syn region aztexCommandE start=/\$ *{/ end=/}/ contains=@aztexBlockTypes,@aztexFuncVars,aztexComment,aztexImport fold
syn region aztexCommandE start=/\$ *[^{ ]/ end=/[ \n{}()$#@]\@=/ contains=@aztexBlockTypes,@aztexFuncVars,aztexComment,aztexImport keepend
syn region aztexTextE start=/@ *{/ end=/}/ contains=@aztexBlockTypes,aztexString,aztexNumber,aztexFloat,aztexComment,@Spell fold
syn region aztexTextE start=/@ *[^{ ]/ end=/[ \n{}()$#@]\@=/ contains=@aztexBlockTypes,aztexString,aztexNumber,aztexComment,aztexFloat,@Spell keepend
syn region aztexMathE start=/# *{/ end=/}/ contains=@aztexBlockTypes,aztexNumber,aztexFloat,aztexComment,@aztexOperator fold
syn region aztexMathE start=/# *[^{ ]/ end=/[ \n{}$#@]\@=/ contains=@aztexBlockTypes,aztexNumber,aztexFloat,aztexComment,@aztexOperator keepend
syn region aztexBlock start=/\([@$#] *\)\@<!{/ end=/}/ fold transparent
syn cluster aztexBlockTypes contains=aztexCommandE,aztexTextE,aztexMathE,aztexBlock

" Strings
syn region aztexString  start=/``/  end=/''/  contains=aztexLineContinuation,@Spell
"syn region aztexString  start=/"/  end=/"/  skip=/\\./re=e+1 contains=aztexLineContinuation,@Spell

syn region aztexImport start=/import / end=/$/ contains=aztexKeyword,aztexString

" Standard numbers
syn match aztexNumber  "\<\d\+[ij]\?\>"
" Floating point number, with dot, optional exponent
syn match aztexFloat   "\<\d\+\(\.\d*\)\?\([edED][-+]\?\d\+\)\?[ij]\?\>"
" Floating point number, starting with a dot, optional exponent
syn match aztexFloat   "\.\d\+\([edED][-+]\?\d\+\)\?[ij]\?\>"

" Line continuations, order of matches is important here
"syn match aztexLineContinuation  "\.\{3}$"
"syn match aztexLineContinuation  "\\$"
"syn match aztexError  "\.\{3}.\+$"hs=s+3
"syn match aztexError  "\\\s\+$"hs=s+1
" Line continuations w/comments
"syn match aztexLineContinuation  "\.\{3}\s*[#%]"me=e-1
"syn match aztexLineContinuation  "\\\s*[#%]"me=e-1

" Comments, order of matches is important here
syn match  aztexComment  "%.*$"  contains=@Spell
" TODO
"syn region aztexBlockComment  start="%{"  end="}%" contains=@Spell

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Apply highlight groups to syntax groups defined above

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_aztex_syntax_inits")
  if version < 508
    let did_aztex_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  highlight MathVar cterm=bold

  HiLink aztexKeyword                  Keyword
  HiLink aztexImport                   Include
  HiLink aztexPredef                   Function
  HiLink aztexFunction                 Function
  HiLink aztexLatex                    Function
  HiLink aztexString                   Label
  HiLink aztexTextE                    String
  HiLink aztexCommandE                 Identifier
  HiLink aztexMathE                    MathVar
  HiLink aztexNumber                   Number
  HiLink aztexFloat                    Float
  "HiLink aztexError                    Error
  HiLink aztexLogicalOperator          Operator
  HiLink aztexArithmeticOperator       Operator
  HiLink aztexRelationalOperator       Operator
  HiLink aztexComment                  Comment
  HiLink aztexBlockComment             Comment
  "HiLink aztexLineContinuation         Special

  delcommand HiLink
endif

let b:current_syntax = "aztex"

