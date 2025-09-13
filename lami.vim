if exists("b:current_syntax")
  finish
endif

syntax keyword lamiKeyword let if then else nil self true false

syntax match lamiIdentifier /\v<[a-zA-Z_][a-zA-Z0-9_]*>/
syntax match lamiIdentifier /:\S\+/ contained

syntax match lamiNumber /\v\<\d+\>/

syntax match lamiOperator /\\/
syntax match lamiOperator /->/

syntax match lamiDelimiter /[()]/ contained

highlight default link lamiOperator Operator
highlight default link lamiKeyword Keyword
highlight default link lamiIdentifier Identifier
highlight default link lamiNumber Number
highlight default link lamiDelimiter Delimiter

let b:current_syntax = "lami"
