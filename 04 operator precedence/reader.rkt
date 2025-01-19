#lang racket

; This is almost exactly as the boilerplate in the previous section.
(require syntax/strip-context)
(require "parser.rkt" "tokenize.rkt" "repl.rkt")

; A slight deviation here is that we define our reader with it's own
;  name and then rename it as it is exported.
(provide (rename-out [thisreader read-syntax]))

; Here the only real change is that we have the module include racket
;  and explictly require the expander.
; Since we do not have any concerns about controlling access to functions
;  there was really no reason to intercept and override things such as
;  #%module-begin.
(define (thisreader path port)
  (define inp (parse (tokenize port)))
  (strip-context
   #`(module calculemus racket
       (require "expander.rkt")
       #,inp
       )))