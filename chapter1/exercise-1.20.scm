; Exercise 1.20
; The process that a procedure generates is of course dependent on the rules
; used by the interpreter. As an example, consider the iterative gcd procedure
; given above. Suppose we were to interpret this procedure using normal-order
; evalution, as discussed in Section 1.1.5. (The normal-order-evaluation rule
; for if is described in Exercise 1.5.) Using the substitution method (for
; normal order), illustrate the process generated in evaluating (gcd 206 40)
; and indicate the remainder operations that are actually performed. How many
; remainder operations are actually performed in the normal-order evalution of
; (gcd 206 40)? In the applicative-order evalution?

(define (gcd x y)
        (if (= y 0)
            x
            (gcd y (remainder x y))))

; Test cases
(gcd 206 40)

; Normal order evalution
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(gcd 6 (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40)) (remainder 6 (remainder 40 (remainder 206 40))))
(gcd (remainder 40 6) (remainder 6 (remainder 40 (remainder 206 40))))
(gcd 4 (remainder 6 (remainder 40 (remainder 206 40))))
(gcd (remainder 6 (remainder 40 (remainder 206 40))) (remainder 4 (remainder 6 (remainder 40 (remainder 206 40)))))
(gcd (remainder 6 (remainder 40 6)) (remainder 4 (remainder 6 (remainder 40 (remainder 206 40)))))
(gcd (remainder 6 4) (remainder 4 (remainder 6 (remainder 40 (remainder 206 40)))))
(gcd 2 (remainder 4 (remainder 6 (remainder 40 (remainder 206 40)))))
(gcd 2 (remainder 4 (remainder 6 (remainder 40 6))))
(gcd 2 (remainder 4 (remainder 6 4)))
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
; 10 times

; Applicative order evalution
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
; 4 times
