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

syn keyword  smllKeyword  fun type struct enum using java as
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
syn match    smllOperator     "\?"
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

hi def link smllComment      Comment

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
