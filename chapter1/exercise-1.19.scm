; Exercise 1.19
; These is a clever algorithm for computing the Fibonacci numbers in a
; logarithmic number of steps. Recall the transformation of the state variables
; a and b in the fib-iter process of Section 1.2.2: a <- a + b and b <- a. Call
; this transformation T, and observe that applying T over and over again n
; times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). In
; other words, the Fibonacci numbers are produced by applying T^n, the nth
; power of the transformation T, starting with the pair (1, 0). Now consider T
; to be the special case of p = 0 and q = 1 in a family of transformations Tpq,
; where Tpq transforms the pair (a, b) according to a <- bq + aq + ap and b <-
; bp + aq. Show that if we apply such a transformation Tpq twice, the effect is
; the same as using a single transformation Tp'q' of the same form, and compute
; p' and q' in terms of p and q. This gives us an explicit way to square these
; transformations, and thus we can compute T^n using successive squaring, as
; in the fast-expt procedure. Put this all together to complete the following
; procedure, which runs in a logarithmic number of steps:

(define (fib n)
        (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
        (cond ((= count 0) b)
              ((even? count)
               (fib-iter a
                         b
                         <??> : compute p'
                         <??> : compute q'
                         (/ count 2)))
              (else (fib-iter (+ (* b q) (* a q) (* a p))
                              (+ (* b p) (* a q))
                              p
                              q
                              (- count 1)))))

; Tpq   -> a <- bq + aq + ap
;          b <- bp + aq
;
; Tpq^2 -> a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;               b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
;          b <- (bp + aq)p + (bq + aq + ap)q
;               b(p^2 + q^2) + a(q^2 + 2pq)
;
; p' = (p^2 + q^2)
; q' = (q^2 + 2pq)

(define (square x) (* x x))
(define (fib n)
        (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
        (cond ((= count 0) b)
              ((even? count)
               (fib-iter a
                         b
                         (+ (square p)
                            (square q))
                         (+ (square q)
                            (* 2 p q))
                         (/ count 2)))
              (else (fib-iter (+ (* b q) (* a q) (* a p))
                              (+ (* b p) (* a q))
                              p
                              q
                              (- count 1)))))

; Test cases
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)
(fib 8)
(fib 9)
(fib 10)
