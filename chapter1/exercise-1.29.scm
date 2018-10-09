; Exercise 29
; Simpson's rule is a more accurate method of numerical integration that the
; method illustrated above. Using Simpson's Rule, the integral of a function f
; between a and b is approximated as
; h/3(y0 + 4y1 + 2y2 + 4y3 + 2y4 + ... + 2yn-2 + 4yn-2 + yn),
; where h = (b - a) / n, for some even integer n, and yk = f(a + kh).
; (Increasing n increases the accuracy of the approximation.) Define a
; procedure that takes as arguments f, a, b, and n and returns the value of the
; integral, computed using Simpson's Rule. Use your procedure to integrate cube
; between 0 and 1 (with n = 100 and n = 1000), and compare the results to those
; of the integral procedure shown above.

(define (even? a) (= (remainder a 2) 0))
(define (cube x) (* x x x))

(define (sum term a next b)
        (if (> a b)
            0
            (+ (term a)
               (sum term (next a) next b))))

(define (integral f a b n)
        (define h (/ (- b a) n))
        (define (y k)
                (f (+ a (* k h))))
        (define (term a)
                (cond ((or (= a 0) (= a n))
                       (y a))
                      ((even? a)
                       (* 2 (y a)))
                      (else
                       (* 4 (y a)))))
        (* (/ h 3.0)
           (sum term a 1+ n)))

(integral cube 0 1 100)
(integral cube 0 1 1000)
