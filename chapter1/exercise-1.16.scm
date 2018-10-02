; Exercise 1.16
; Design a procedure that evolves an iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps, as does
; fast-expt.  (Hint: Using the observation that (b^n/2)^2 = (b^2)^n/2, keep,
; along with the exponent n and the base b, an additional state variable a, and
; define the state transformation in such a way that the product a.b^n is
; unchanged from state to state.  At the beginning of the process a is taken to
; be 1, and the answer is given by the value of a at the end of the process. In
; general, the technique of defining an invariant quantity that remains
; unchanged from state to state is a powerful way to think about the design of
; iterative algorithms.

(define (fast-expt b n)
        (define (even? x)
                (= (remainder x 2) 0))
        (define (expt-iter b n a)
                (cond ((= n 0) a)
                      ((even? n) (expt-iter (* b b) (/ n 2) a))
                      (else (expt-iter b (- n 1) (* a b)))))
        (expt-iter b n 1))

; Test cases
(fast-expt 2 0)
(fast-expt 2 1)
(fast-expt 2 2)
(fast-expt 2 3)
(fast-expt 3 3)
(fast-expt 5 3)
(fast-expt 5 4)
(fast-expt 5 5)
