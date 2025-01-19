#lang brag

; Exprs form the basic unit of our grammar.
; These are one or more expressions (at level 3) linked toegether
;  by semicolons.
; Note that the forward slash before the semicolon (and elsewhere
;  below) indicate that the parser should use this for parsing but
;  discard it from the final syntax tree.
; This is called a 'cut' and is a feature of brag that allows us to
;  do some house cleaning on our syntax tree before we even see it.
Exprs: Expr3 (/";" Expr3)*

; To implmenent operator precedence, we have a hierarchy of expressions.
; A level n+1 expression is either a level n expression, or it is a
;  level n expression combined with another lebel n+1 expression via a
;  leven n+1 operator.
; A level 0 expression is either a number, a negated level 3 expression,
;  or a level 3 expression within a parenthetical grouping.
Expr3: Expr3 Op3 Expr2 | Expr2
Expr2: Expr2 Op2 Expr1 | Expr1
Expr1: Expr1 Op1 Expr0 | Expr0
Expr0: NUM | /'(' Expr3 /')' | "-" Expr3

; Finally we organize our operators by precedence.
Op1: '^' 
Op2: '*' | '/'
Op3: '+' | '-'