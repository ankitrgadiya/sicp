; Exercise 1.7
; The good-enough? test used in computing square roots will not be very
; effective for finding the square roots of very small numbers. Also, in real
; computers, arithmetic operations are almost always performed with limited
; precision. This makes our test inadequate for very large numbers. Explain
; these statements, with exaplmes showing how the test fails for small and
; large numbers. An alternative strategy for implementing good-enough? is to
; watch how guess changes from one iteration to the next and to stop when the
; change is a very small fraction of the guess. Design a square-root procedure
; that uses this kind of end test. Does this work better for small and large
; numbers?

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
(define (sqrt-iter guess x)
        (if (good-enough? guess x)
                guess
                (sqrt-iter (improve guess x) x)))

; good-enough? test works for normal numbers like 9 or small numbers like 0.04
(sqrt 9)
; correct
(sqrt 0.04)
; correct

; But as we have hard coded the value 0.001 we certainly cannot use it for
; numbers smaller than that.
(sqrt 0.000016)
; incorrect

; Also, as the language itself uses limited precision to represent numbers, it
; cannot computer the square root of numbers larger than the language supports.
(sqrt 1230971249071092479012749012749071249012790347)
; incorrect

(define (good-enough? new-guess old-guess)
        (< (abs (- new-guess old-guess)) 0.001))

(define (square-root guess x)
        (define new-guess (improve guess x))
        (if (good-enough? new-guess guess)
            new-guess
            (square-root new-guess x)))

(define (sqrt x)
        (square-root 1.0 x))

(sqrt 9)
; correct
(sqrt 0.04)
; correct
(sqrt 0.000016)
; correct
(sqrt 1230971249071092479012749012749071249012790347)
; incorrect
