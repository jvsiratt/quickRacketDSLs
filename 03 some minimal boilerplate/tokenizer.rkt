#lang racket

; We will be using the brag library for parsing rather than
;  the parsing and lexing included with the standard library.
(require brag/support)

(provide tokenize)

; For the most part, defining our lexer remains the same.
; Some of the new tools we are importing allow us to avoid
;  declaring our tokens before the lexer.
; This also involves some slight changes to the way tokens work.
(define thelexer
  (lexer
   [(eof) (token 'EOF)]
   [(:+ numeric) (token 'NUMBER (string->number lexeme))]
   [":stuff" (token 'STUFF)]
   [(:+ alphabetic) (token 'WORD lexeme)]
   [whitespace (token lexeme #:skip? #t)])
  )

(define (tokenize inp)
  (define (next-token) (thelexer inp))
  next-token)