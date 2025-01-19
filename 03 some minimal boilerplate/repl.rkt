#lang racket

; Here we make a simple REPL reader.
; This does not need to change much between projects aside from
;  what grammar case the parser is generated from.
(require "parser.rkt" "tokenizer.rkt" "expander.rkt")

(define repl-read (make-rule-parser therepl))

(define (read-one-line origin port)
  (define one-line (read-line port))
  (if (eof-object? one-line)
      eof
      (repl-read (tokenize (open-input-string one-line)))))

(current-read-interaction read-one-line)