" Vim syntax file
" Language: Copper
" Maintainer: Chris Dean
" Latest Revision: 21021 Nov 13

if exists("b:current_syntax")
  finish
endif
let b:current_syntax = "terse"

syn keyword terseKeyword fn global class field nonlocal break continue return let var
syn keyword terseConditional while if else and for
syn keyword terseBuiltin print prints
syn keyword terseBool true false
syn keyword terseNull null
syn match terseKeyword '\\'
syn match terseOperator '='
syn match terseOperator '=='
syn match terseOperator '|'
syn match terseOperator '||'
syn match terseOperator '|='
syn match terseOperator '&'
syn match terseOperator '&&'
syn match terseOperator '&='
syn match terseOperator '\^'
syn match terseOperator '\^\^'
syn match terseOperator '\^='
syn match terseOperator '!'
syn match terseOperator '!='
syn match terseOperator '>'
syn match terseOperator '>>'
syn match terseOperator '>='
syn match terseOperator '>>='
syn match terseOperator '<'
syn match terseOperator '<<'
syn match terseOperator '<='
syn match terseOperator '<<='
syn match terseOperator '+'
syn match terseOperator '++'
syn match terseOperator '+='
syn match terseOperator '-'
syn match terseOperator '--'
syn match terseOperator '-='
syn match terseOperator '%'
syn match terseOperator '%='
syn match terseOperator '\*'
syn match terseOperator '\*='
syn match terseOperator '\/'
syn match terseOperator '\/='
syn match terseOperator '\~'
syn match terseOperator '<-'
syn match terseOperator '|>'
syn match terseOperator '->'
syn keyword terseOperator in


syn match terseNumber '\<\d\+\(\.\d\+\)\?'
syn match terseString '"\([^"\\]\|\\.\)*"'
syn match terseChar '\'\([^"\\]\|\\.\)\''
syn match terseLambdaArg '\\\d\+'
syn match terseComment '#.*$'

syn match terseFunction "\<\k\+\ze("

hi def link terseConditional Conditional
hi def link terseKeyword Keyword
hi def link terseBuiltin Function
hi def link terseNumber Number
hi def link terseString String
hi def link terseChar Character
hi def link terseBool Boolean
hi def link terseOperator Operator
hi def link terseNull Constant
hi def link terseLambdaArg PreProc
hi def link terseFunction Function
hi def link terseComment Comment

