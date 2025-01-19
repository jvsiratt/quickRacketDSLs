#lang brag

; brag is parser generator that accepts Backus-Naur style grammar
;  definitions.
; I don't mind the yacc-style parsing in the standard library,
;  but this feels a bit more user-friendly.
; The top item in the grammar is terminal and serves as an entry
;  point for the parser.
; See the brag documentation or the excellent Beautiful Racket
;  tutorial for more information.
topthing: sumthin | thestuff
sumthin: NUMBER | WORD
thestuff : STUFF

; This is not particularly useful here, but defining a repl rule
;  can be useful when the REPL is not expected to handle the
;  syntax.
; brag allows us to create parsers for fragments of the language
;  using a particular entry point into the grammar.
; We can then use these parsers to build custom REPL interactions.
therepl : topthing