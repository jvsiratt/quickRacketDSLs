#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-empty-tokens the-empty-tokens
  (EOF))
(define-tokens the-tokens
  (WORD))

(define word-list-lex
  (lexer
   [(eof) (token-EOF)]
   [(:+ alphabetic) (token-WORD lexeme)]
   [(::";;" any-string ";;") (word-list-lex input-port)]
   [whitespace (word-list-lex input-port)]))

(define (word-list-tokenize inp)
  (define (next-token) (word-list-lex inp))
  next-token)

(define word-list-parse
  (parser
   [start list-of-words]
   [end EOF]
   [error void]
   [tokens the-empty-tokens the-tokens]
   [grammar
    [list-of-words [(WORD list-of-words) (cons $1 $2)]
                   [() empty]]]))

(require syntax/strip-context)
(define (read-syntax path port)
  (define inp (word-list-parse (word-list-tokenize port)))
  (strip-context
   #`(module word-file-module racket
       (define word-list '#,inp)
       (define (choose-word)
         (if (= 0 (length word-list))
             (error "Word list is empty!")
             (car (shuffle word-list))))
       (provide choose-word))))
(provide read-syntax)