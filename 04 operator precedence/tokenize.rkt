#lang racket

; This is almost word-for-word the way it appears in the previous section's boilerplate.
(require brag/support)

(provide tokenize)

; The only differences is that our lexer is looking for different patterns and
;  supplying different tokens.
; Here we want to recognize numbers, some operations on numbers, parenthetical
;  grouping, and comments.
(define thelexer
  (lexer
   [(eof) (token 'EOF)]
   [(:or (:+ numeric)
         (:: (:? (:+ numeric)) "." (:+ numeric)))
    (token 'NUM (string->number lexeme))]
   [(char-set "^*/+-();") (token lexeme (string->symbol lexeme))]
   [(from/to "%%" "\n") (token 'COMMENT #:skip? #t)]
   [whitespace (token lexeme #:skip? #t)])
  )

(define (tokenize inp)
  (define (next-token) (thelexer inp))
  next-token)