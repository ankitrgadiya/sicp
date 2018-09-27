; Exercise 1.13
; Prove that Fib(n) is the closest integer to φ/√5, where φ = (1 + √5)/2.
; Hint: Let ψ = (1 - √5)/2. Use induction and the definition of the Fibonacci
; numbers to prove that Fib(n) = (φ^n - ψ^n)/√5.

(define phi
        (/ (+ 1
              (sqrt 5))
           2))

(define psi
        (/ (- 1
              (sqrt 5))
           2))

(define (pow base exponent)
        (if (= exponent 0)
            1
            (* base
               (pow base (-1+ exponent)))))

(define (f n)
        (/ (- (pow phi n)
              (pow psi n))
           (sqrt 5)))

; Test cases
(f 0)
(f 1)
(f 2)
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 8)
(f 9)
(f 10)
