#lang racket

; The expander will vary widely between projects.
; This is where we can define what each part of the grammar
;  actually does in Racket terms.
(provide topthing sumthin thestuff therepl stuff)

; Here we provide some necessaries that are usually provided
;  by #lang Racket.
; We could also supply these to the reader by requiring Racket
;  there; it depends on how much we want to control the user's
;  access and experience.
; Providing them here allows us to override them with our own
;  versions if we choose to do so.
(provide #%module-begin #%app #%datum #%top-interaction)

; Here we have defined each part of the grammar as a procedure
;  but often syntax transformations (macros) will be useful here.
(define stuff 0)

(define (topthing x) x)

(define (sumthin x)
  (set! stuff x))

(define (thestuff x) (void))

(define (therepl x)
  (displayln stuff))