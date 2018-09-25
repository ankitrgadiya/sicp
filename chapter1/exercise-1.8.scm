; Exercise 1.8
; Newton's method for cude roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given by
; the value
;
; x/y^2 + 2y
; ----------
;      3
;
; Use this formula to implement a cube-root procedure analogous to the
; sqaure-root procedure. (In Section 1.3.4 we will see how to implement
; Newton's method in general as an abstraction of these sqaure-root and
; cube-root procedures.)

; Required procedures
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x)
        (if (< x 0)
            (- x)
            x))
(define (improve guess x)
        (/ (+ (/ x
                 (square guess))
              (* 2 guess))
           3))
(define (good-enough? new-guess old-guess)
        (< (abs (- new-guess old-guess)) 0.001))
(define (cube-root guess x)
        (define new-guess (improve guess x))
        (if (good-enough? new-guess guess)
            new-guess
            (cube-root new-guess x)))
(define (cbrt x)
        (cube-root 1.0 x))

(cbrt 8)
(cbrt 27)
(cbrt 64)
(cbrt 125)
(cbrt 625)
