; Exercise 1.39
; A continued fraction representation of the tangent function was published in
; 1770 by the German mathematician J.H. Lambert:
; tan x =          x
;         -------------------
;         1 -       x^2
;             ---------------
;             3 -      x^2
;                 -----------
;                 5 - .......
; where x is in radians. Define a procedure (tan-cf x k) that computes an
; approximation to the tangent function based on Lambert's formula. k specifies
; the number of terms to compute, as in Exercise 1.37.

(define (cont-frac n d k)
        (define (iter k result)
                (if (= k 0)
                    result
                    (iter (-1+ k)
                          (/ (n k)
                             (+ (d k) result)))))
        (iter k 0))

(define (tan-cf x k)
        (cont-frac
         (lambda (i)
                 (if (= i 1)
                     x
                     (- (square x))))
         (lambda (i)
                 (- (* 2 i) 1.0))
         k))

(tan-cf 3 10)
