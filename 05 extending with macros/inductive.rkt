#lang typed/racket

; Using require makes imports available at the runtime where regular
;  code lives.
; Macro expansion precedes this. We need syntax/parse and racket/syntax
;  for macros, we use for-syntax with require to import them at the
;  appropriate phase.
(require (for-syntax syntax/parse
                     racket/syntax))

; We will define simple implementation of inductive types by leveraging
;  structs and inhertiance.
; For our inductive type we will define a struct with no fields, however
;  this defines an unneeded constructor which we will rename.
(define-syntax (Ind syn)
  (syntax-parse syn
    [(_ name condata ...)
     (with-syntax
         ([arbcon (format-id #'name "arbitrary-~a" #'name)])
       #'(begin
           (struct name ()
             #:transparent
             #:constructor-name arbcon)
           (Konstruct condata name)
           ...))]))

; For the actual type constructors we define structs with the appropriate
;  fields using the inductive type struct as the super.
(define-syntax (Konstruct syn)
  (syntax-parse syn
    [(_ (name) sup)
     #'(struct name sup () #:transparent)]
    [(_ (name [nm : ty] ...) sup)
     #'(struct name sup
         ([nm : ty]
          ...)
         #:transparent)]))

; As an example, we define the naturals abstractly with two
;  constructors -- a nullary constructor representing zero, and a
;  unary constructor for creating successors.
; The definition for Succ also includes an accessor that returns the
;  predecessor of a natural.
(Ind Nat
     (Zero)
     (Succ [pred : Nat]))

; Because of our design choices, we can create non-standard naturals
;  like (Succ (arbitrary-Nat).
; This makes sense a bit of sense, but it eliminates a defining
;  property of inductive types -- well-foundedness.
; What we would like is to be able to define functions on Nats that
;  only need to consider the two meaninful cases, as below.
(: unsafe-addNat (-> Nat Nat Nat))
(define (unsafe-addNat n m)
  (match n
    [(Zero) m]
    [(Succ x) (unsafe-addNat x (Succ m))]))

; We could just add a case above for (abitrary-Nat), but it wouldn't
;  catch a non-well-founded m.
; We can easily sketch out a procedure to determine if an instance
;  of Nat is well-founded.
(: well-founded-Nat? (-> Nat Boolean))
(define (well-founded-Nat? n)
  (match n
    [(Succ x) (well-founded-Nat? x)]
    [(Zero) #t]
    [(Nat) #f]))

; Using the well-foundedness test, we can now wrap our original
;  procedure with some guards.
(: add-Nat (-> Nat Nat Nat))
(define (add-Nat n m)
  (if (and (well-founded-Nat? n)
           (well-founded-Nat? m))
      (unsafe-addNat n m)
      (error "Both parameters must be well-founded.")))