syn match smllComment "(\*"
syntax region smllComment start="(\*" end="\*)"
hi def link smllComment Comment

syn match smllCArrayModifier "'[cC]"
hi def link smllCArrayModifier StorageClass

" true, false, nil
syn keyword smllLiteralKeyword true false nil
hi def link smllLiteralKeyword Boolean

" statements
syn keyword smllConditionalKeyword defer else if match
hi def link smllConditionalKeyword Keyword

" syn keyword smllLoopKeyword for while loop
" hi def link smllLoopKeyword Repeat

" syn keyword smllLabelKeyword break continue
" hi def link smllLabelKeyword Label

syn match smllEmptyOption "_\ze[^a-zA-Z0-9?'_]"
hi def link smllEmptyOption Label

" syn keyword smllOperatorKeyword as
" hi def link smllOperatorKeyword Operator

syn match smllOperators "[!+\-*/=%&|<>?]"
hi def link smllOperators Operator

syn match smllParens "[\[\](){}]"
hi def link smllParens Ignore

syn keyword smllOtherKeyword java
hi def link smllOtherKeyword Keyword

" top-level keywords
syn keyword smllTopLevelKeyword fun using type enum struct val let
hi def link smllTopLevelKeyword Keyword

" macros
syn match smllMacroIdent "[a-zA-Z_][a-zA-Z0-9?'_]*!"
hi def link smllMacroIdent Macro

" syn keyword smllPreprocessorKeyword macro import
" hi def link smllPreprocessorKeyword PreProc

" default types
syn keyword smllPrimitiveType Unit Int Float Char Double Long Short
hi def link smllPrimitiveType Type

" syn keyword smllStructure enum interface struct union
" hi def link smllStructure Structure

" syn keyword smllTypeModifier const embed extern let
" hi def link smllTypeModifier Statement

syn match smllTypedef "[A-Z][a-zA-Z0-9?'_]*"
hi def link smllTypedef Typedef

" function calls
syn match smllFnIdent "[a-zA-Z_][a-zA-Z0-9_]*\s*\ze("
hi def link smllFnIdent Function

" number literals:
syn match smllNumber "[0-9][0-9_]*"
hi def link smllNumber Number

syn match smllHexNumber "0[xX][0-9a-fA-F][0-9a-fA-F_]*"
hi def link smllHexNumber Number

syn match smllBinNumber "0[bB][01][01_]*"
hi def link smllBinNumber Number

syn match smllOctNumber "0[oO][0-7][0-7_]*"
hi def link smllOctNumber Number

syn match smllFltNumber "[0-9][0-9_]*\.[0-9_][0-9_]*"
hi def link smllFltNumber Float

" strings
syn match smllEscapedChar "'\\.'" contained
hi def link smllEscapedChar SpecialChar

syn region smllString start=+"+ end=+"+ skip=+\\"+ contains=smllEscapedChar
hi def link smllString String

syn match smllChar "'.'"
syn match smllChar "'\\.'"
hi def link smllChar Character

