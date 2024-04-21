" Vim syntax file
" Language:     SMLL
" Filenames:    *.smlL


" quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" Disable spell checking of syntax.
syn spell notoplevel

" smll is case sensitive.

" lowercase identifier - the standard way to match
" syn match    smllLCIdentifier /\<\(\l\|_\)\(\w\|'\)*\>/

syn match    smllKeyChar    "|"

" Errors
syn match    smllBraceErr   "}"
syn match    smllBrackErr   "\]"
syn match    smllParenErr   ")"
syn match    smllCommentErr "\*)"
syn match    smllThenErr    "\<then\>"

" Error-highlighting of "end" without synchronization:
" as keyword or as error (default)
if exists("smll_noend_error")
  syn match    smllKeyword    "\<end\>"
else
  syn match    smllEndErr     "\<end\>"
endif

" Some convenient clusters
syn cluster  smllAllErrs contains=smlBraceErr,smlBrackErr,smlParenErr,smlCommentErr,smlEndErr,smlThenErr

syn cluster  smllAENoParen contains=smlBraceErr,smlBrackErr,smlCommentErr,smlEndErr,smlThenErr

syn cluster  smllContained contains=smlTodo,smlPreDef,smlModParam,smlModParam1,smlPreMPRestr,smlMPRestr,smlMPRestr1,smlMPRestr2,smlMPRestr3,smlModRHS,smlFuncWith,smlFuncStruct,smlModTypeRestr,smlModTRWith,smlWith,smlWithRest,smlModType,smlFullMod


" Enclosing delimiters
syn region   smllEncl transparent matchgroup=smlKeyword start="(" matchgroup=smlKeyword end=")" contains=ALLBUT,@smlContained,smlParenErr
syn region   smllEncl transparent matchgroup=smlKeyword start="{" matchgroup=smlKeyword end="}"  contains=ALLBUT,@smlContained,smlBraceErr
syn region   smllEncl transparent matchgroup=smlKeyword start="\[" matchgroup=smlKeyword end="\]" contains=ALLBUT,@smlContained,smlBrackErr
syn region   smllEncl transparent matchgroup=smlKeyword start="#\[" matchgroup=smlKeyword end="\]" contains=ALLBUT,@smlContained,smlBrackErr


" Comments
syn region   smllComment start="(\*" end="\*)" contains=smlComment,smlTodo,@Spell
syn keyword  smllTodo contained TODO FIXME XXX


" let
syn region   smllEnd matchgroup=smlKeyword start="\<let\>" matchgroup=smlKeyword end="\<end\>" contains=ALLBUT,@smlContained,smlEndErr

" if
syn region   smllNone matchgroup=smlKeyword start="\<if\>" matchgroup=smlKeyword end="\<then\>" contains=ALLBUT,@smlContained,smlThenErr


"" Modules

" "struct"
" syn region   smllStruct matchgroup=smlModule start="\<struct\>" matchgroup=smlModule end="\<end\>" contains=ALLBUT,@smlContained,smlEndErr

" "sig"
" syn region   smllSig matchgroup=smlModule start="\<sig\>" matchgroup=smlModule end="\<end\>" contains=ALLBUT,@smlContained,smlEndErr,smlModule
" syn region   smllModSpec matchgroup=smlKeyword start="\<structure\>" matchgroup=smlModule end="\<\u\(\w\|'\)*\>" contained contains=@smlAllErrs,smlComment skipwhite skipempty nextgroup=smlModTRWith,smlMPRestr

" "open"
"syn region   smllNone matchgroup=smlKeyword start="\<open\>" matchgroup=smlModule end="\<\w\(\w\|'\)*\(\.\w\(\w\|'\)*\)*\>" contains=@smlAllErrs,smlComment

" "structure" - somewhat complicated stuff ;-)
" syn region   smllModule matchgroup=smlKeyword start="\<\(structure\|functor\)\>" matchgroup=smlModule end="\<\u\(\w\|'\)*\>" contains=@smlAllErrs,smlComment skipwhite skipempty nextgroup=smlPreDef
" syn region   smllPreDef start="."me=e-1 matchgroup=smlKeyword end="\l\|="me=e-1 contained contains=@smlAllErrs,smlComment,smlModParam,smlModTypeRestr,smlModTRWith nextgroup=smlModPreRHS
" syn region   smllModParam start="([^*]" end=")" contained contains=@smlAENoParen,smlModParam1
" syn match    smllModParam1 "\<\u\(\w\|'\)*\>" contained skipwhite skipempty nextgroup=smlPreMPRestr

" syn region   smllPreMPRestr start="."me=e-1 end=")"me=e-1 contained contains=@smlAllErrs,smlComment,smlMPRestr,smlModTypeRestr
" 
" syn region   smllMPRestr start=":" end="."me=e-1 contained contains=@smlComment skipwhite skipempty nextgroup=smlMPRestr1,smlMPRestr2,smlMPRestr3
" syn region   smllMPRestr1 matchgroup=smlModule start="\ssig\s\=" matchgroup=smlModule end="\<end\>" contained contains=ALLBUT,@smlContained,smlEndErr,smlModule
" syn region   smllMPRestr2 start="\sfunctor\(\s\|(\)\="me=e-1 matchgroup=smlKeyword end="->" contained contains=@smlAllErrs,smlComment,smlModParam skipwhite skipempty nextgroup=smlFuncWith
" syn match    smllMPRestr3 "\w\(\w\|'\)*\(\.\w\(\w\|'\)*\)*" contained
" syn match    smllModPreRHS "=" contained skipwhite skipempty nextgroup=smlModParam,smlFullMod
" syn region   smllModRHS start="." end=".\w\|([^*]"me=e-2 contained contains=smlComment skipwhite skipempty nextgroup=smlModParam,smlFullMod
" syn match    smllFullMod "\<\u\(\w\|'\)*\(\.\u\(\w\|'\)*\)*" contained skipwhite skipempty nextgroup=smlFuncWith
" 
" syn region   smllFuncWith start="([^*]"me=e-1 end=")" contained contains=smlComment,smlWith,smlFuncStruct
" syn region   smllFuncStruct matchgroup=smlModule start="[^a-zA-Z]struct\>"hs=s+1 matchgroup=smlModule end="\<end\>" contains=ALLBUT,@smlContained,smlEndErr
" 
" syn match    smllModTypeRestr "\<\w\(\w\|'\)*\(\.\w\(\w\|'\)*\)*\>" contained
" syn region   smllModTRWith start=":\s*("hs=s+1 end=")" contained contains=@smlAENoParen,smlWith
" syn match    smllWith "\<\(\u\(\w\|'\)*\.\)*\w\(\w\|'\)*\>" contained skipwhite skipempty nextgroup=smlWithRest
" syn region   smllWithRest start="[^)]" end=")"me=e-1 contained contains=ALLBUT,@smlContained

" "signature"
" syn region   smllKeyword start="\<signature\>" matchgroup=smlModule end="\<\w\(\w\|'\)*\>" contains=smlComment skipwhite skipempty nextgroup=smlMTDef
" syn match    smllMTDef "=\s*\w\(\w\|'\)*\>"hs=s+1,me=s

syn keyword  smllKeyword  fun type struct enum using java
syn keyword  smllKeyword  val let in if then else match fn null of 

syn keyword  smllType     Int List Any Float Double Char Unit String Bool Long Short Byte Result Option Either


syn keyword  smllBoolean      true false
syn match    smllConstructor  "(\s*)"
syn match    smllConstructor  "\[\s*\]"
syn match    smllConstructor  "#\[\s*\]"
syn match    smllConstructor  "\u\(\w\|'\)*\>"

syn match smllFnIdent "[a-zA-Z_][a-zA-Z0-9_]*\s*\ze("

" Module prefix
syn match    smllModPath      "\u\(\w\|'\)*\."he=e-1

syn match    smllCharacter    +#"\\""\|#"."\|#"\\\d\d\d"+
syn match    smllCharErr      +#"\\\d\d"\|#"\\\d"+
syn region   smllString       start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=@Spell

syn match    smllFunDef       "=>"
syn match    smllOperator     "\^"
syn match    smllOperator     "::"
syn match    smllAnyVar       "\<_\>"
syn match    smllKeyChar      "!"
syn match    smllKeyChar      ";"
syn match    smllKeyChar      "\*"
syn match    smllKeyChar      "="

syn match    smllNumber        "\<-\=\d\+\>"
syn match    smllNumber        "\<-\=0[x|X]\x\+\>"
syn match    smllReal          "\<-\=\d\+\.\d*\([eE][-+]\=\d\+\)\=[fl]\=\>"

" Synchronization
syn sync minlines=20
syn sync maxlines=500

" syn sync match smllStructSync  grouphere  smlStruct  "\<struct\>"
" syn sync match smllStructSync  groupthere smlStruct  "\<end\>"
" syn sync match smllSigSync     grouphere  smlSig     "\<sig\>"
" syn sync match smllSigSync     groupthere smlSig     "\<end\>"
" 
" Define the default highlighting.
" Only when an item doesn't have highlighting yet

" hi def link smllBraceErr     Error
" hi def link smllBrackErr     Error
" hi def link smllParenErr     Error

" hi def link smllCommentErr   Error
" 
" hi def link smllEndErr       Error
" hi def link smllThenErr      Error
" 
" hi def link smllCharErr      Error
" 
hi def link smllComment      Comment

 " 
hi def link smllModPath      Include
hi def link smllModule       Include
hi def link smllModParam1    Include
hi def link smllModType      Include
hi def link smllMPRestr3     Include
hi def link smllFullMod      Include
hi def link smllModTypeRestr Include
hi def link smllWith         Include
hi def link smllMTDef        Include

hi def link smllConstructor  Constant

hi def link smllModPreRHS    Keyword
hi def link smllMPRestr2     Keyword
hi def link smllKeyword      Keyword
hi def link smllFunDef       Keyword
hi def link smllRefAssign    Keyword
hi def link smllKeyChar      Keyword
hi def link smllAnyVar       Keyword
hi def link smllTopStop      Keyword
hi def link smllOperator     Keyword

hi def link smllBoolean      Boolean
hi def link smllCharacter    Character
hi def link smllNumber       Number
hi def link smllReal         Float
hi def link smllString       String
hi def link smllType         Type
hi def link smllTodo         Todo
hi def link smllEncl         Keyword
hi def link smllFnIdent      Function

let b:current_syntax = "smll"

" vim: ts=8
