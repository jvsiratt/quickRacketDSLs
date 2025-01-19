#lang racket

; Identical to the boilerplate aside from the definition of repl-read.
(require "parser.rkt" "tokenize.rkt" "expander.rkt")

(define repl-read (make-rule-parser Expr3))

(define (read-one-line origin port)
  (define one-line (read-line port))
  (if (eof-object? one-line)
      eof
      (repl-read (tokenize (open-input-string one-line)))))

(current-read-interaction read-one-line)