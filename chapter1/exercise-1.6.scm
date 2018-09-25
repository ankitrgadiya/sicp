; Exercise 1.6
; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
; "Why can'it I just define it as an ordinary procedure in terms of cond?" she
; asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she
; defines a new version of if:

(define (new-if predicate then-clause else-clause)
        (cond (predicate then-clause)
              (else else-clause)))

; Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
; 5

(new-if (= 1 1) 0 5)
; 0

; Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (sqrt-iter guess x)
        (new-if (good-enough? guess x)
                guess
                (sqrt-iter (improve guess x) x)))

; What happens when Alyssa attems to use this to computer square roots?
; Explain.

; Required procedures
(define (square x) (* x x))
(define (abs x)
        (if (< x 0)
            (- x)
            x))
(define (average x y)
        (/ (+ x y) 2))
(define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
        (average guess (/ x guess)))
(define (sqrt x)
        (sqrt-iter 1.0 x))

(sqrt 9)

; As Scheme interpreter uses applicative-order evaluation, it tries to evaluate
; arguments of new-if even before applying the procedure. This works fine in
; the demonstration because arguments were primitives. But in sqrt-iter
; procedure, this creates a problem because one of the argument is sqrt-iter
; itself. So, even if the predicate is true it will still keep on evaluating
; the arguments infinitely until interpreter aborts saying "maximum recursion
; depth exceeded".
