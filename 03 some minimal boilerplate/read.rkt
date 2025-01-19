#lang racket

; The purpose of the reader is to provide a module into which the
;  parse-tree can be expanded and executed.
; This will then be used by Racket to construct the Racket
;  program that is ran from a file using this reader.
(require syntax/strip-context)
(require "parser.rkt" "tokenizer.rkt" "repl.rkt")

(provide read-syntax)

; The reading module is the program that is ran when a file uses
;  this reader.
; The parse-tree generated from the file is inserted at inp,
;  then expanded into Racket code.
(define (read-syntax path port)
  (define inp (parse (tokenize port)))
  (strip-context
   #`(module reading-module "expander.rkt" #,inp)))