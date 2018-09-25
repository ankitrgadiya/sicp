; Exercise 1.5
; Ben Bitdiddle has invented a test to determine whether the interpreter he is
; faced with is using applicative-order evaluation or normal-order evaluation.
; He defines the following two procedures:

(define (p) (p))
(define (test x y)
        (if (= x 0) 0 y))

; Then he evaluates the expression

(test 0 (p))

; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior will he observe with an
; interpreter that uses normal-order evaluation? Explain your answer. (Assume
; that the evaluation rule for the special form if is the same whether the
; interpreter is using normal or applicative order: The predicate expression is
; evaluated first, and the result determines whether to evaluate the consequent
; or the alternative expression.)

; The p procedure is defines to call itself in a recursive manner and test
; procedure is defined as a simple if conditional which checks if the value of
; first argument is 0 and if so returns 0 otherwise returns second argument.
;
; If the interpreter uses applicative-order evaluation, it will try to evaluate
; p before applying test, which is defined recursively and hence it will keep on
; evaluating p and will never return anything.
;
; If the interpreter uses normal-order evaluation, then it will not try to
; evaluate p until it is required and hence it will go on to check the value of
; first argument which in this case is 0 and will return 0 without even
; evaluating p once.
