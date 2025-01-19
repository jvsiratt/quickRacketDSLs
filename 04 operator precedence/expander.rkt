#lang racket

; Using syntax/parse will demonstrate how to use it for some precise pattern matching, but since
;  the imports are being used for expansions, we need to indicate that with the 'for-syntax' in
;  the require statement.
; This is related to what Racket calls 'phase levels.'
(require (for-syntax syntax/parse))

(provide (all-defined-out))

; The first macro has two pattern matching cases that correspond with the two possibilities
;  allowed in the grammar.
; If the Exprs given are a single expression, then our resulting Racket program will just
;  be based on that expression.
; If the Exprs given are more than one expression, then the resulting program will execute
;  their expansions sequentially.
(define-syntax Exprs
  (syntax-rules ()
    [(_ EXPR) EXPR]
    [(_ EXPR ...) (begin EXPR ...)]))

; Although Expr0 had three possible forms in the grammar, discarding the parenthesis with cuts
;  allows us to treat the parenthetical case in the same way we would as a number -- we just
;  want their expansions without anything extra.
; In the case of unary negation, the macro expansion applies the Racket minus operator to the
;  expansion of the remaining term.
(define-syntax (Expr0 caller-stx)
  (syntax-parse caller-stx
    [(_ (~literal -) EXPR) #'(- EXPR)]
    [(_ EXPR) #'EXPR]))

; For the rest of the Expr's from the grammar, we pattern match on the two corresponding cases.
; Either an ExprN is a single expression requiring expansion, in which case we need do nothing,
;  or it is an operator applied to two expressions.
; In the latter case the macro expansion rearranges things so that the operator expansion will
;  preceed the expansions of the parameters (as Racket expects).
(define-syntax (Expr1 caller-stx)
  (syntax-parse caller-stx
    [(_ EXPRa OP EXPRb) #'(OP EXPRa EXPRb)]
    [(_ EXPR) #'EXPR]))
(define-syntax (Expr2 caller-stx)
  (syntax-parse caller-stx
    [(_ EXPRa OP EXPRb) #'(OP EXPRa EXPRb)]
    [(_ EXPR) #'EXPR]))
(define-syntax (Expr3 caller-stx)
  (syntax-parse caller-stx
    [(_ EXPRa OP EXPRb) #'(OP EXPRa EXPRb)]
    [(_ EXPR) #'EXPR]))

; Finally we take care of the operations.
; In most cases we simply match what symbol is used and pass it on as the expansion; however,
;  in the case of exponentiation, Racket does not use the caret so we need to provide an
;  appropriate function that can be applied to the parameters in the expansion.
(define-syntax (Op1 caller-stx)
  (syntax-parse caller-stx
    [(_ (~literal ^)) #'(λ (x y) (expt x y))]))
(define-syntax (Op2 caller-stx)
  (syntax-parse caller-stx
    [(_ (~literal *)) #'*]
    [(_ (~literal /)) #'/]))
(define-syntax (Op3 caller-stx)
  (syntax-parse caller-stx
    [(_ (~literal +)) #'+]
    [(_ (~literal -)) #'-]))