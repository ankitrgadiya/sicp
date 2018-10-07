; Exercise 1.24
; Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime?
; (the Fermat method), and test each of the 12 primes you found in that
; exercise. Since the Fermat test has Θ(log n) growth, how would you expect the
; time to test primes near 1,000,000 to compare with the time needed to test
; primes near 1000? Do your data bear this out? Can you explain any discrepancy
; you find?

(define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
        (if (fast-prime? n 2)
            (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time))

(define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)  (remainder (square (expmod base (/ exp 2) m)) m))
              (else (remainder (* base (expmod base (- exp 1) m)) m))))
(define (even? x) (= (remainder x 2) 0))
(define (fermat-test n)
        (define (try-it a)
                (= (expmod a n n) a))
        (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
        (cond ((= times 0) true)
              ((fermat-test n) (fast-prime? n (- times 1)))
              (else false)))

(define (search-for-primes a b)
        (define (search-iter a)
                (if (< a b) (timed-prime-test a))
                (if (< a b) (search-iter (+ a 2))))
        (if (= (remainder a 2) 0)
            (search-iter (1+ a))
            (search-iter a)))

; (search-for-primes 1000 1020)
; (search-for-primes 10000 10038)
; (search-for-primes 100000 100044)
; (search-for-primes 1000000 1000038)

; For the values given in question, the computer runs the procedure so fast
; and no data can be obtained. So, I went to higher and higher values to get
; some data.

; (search-for-primes 10000000 10000104)
; (search-for-primes 100000000 100000040)
; (search-for-primes 1000000000 1000000022)
; (search-for-primes 10000000000 10000000062)
; (search-for-primes 100000000000 100000000058)
; (search-for-primes 1000000000000 1000000000064)
; (search-for-primes 10000000000000 10000000000100)
; (search-for-primes 100000000000000 100000000000100)
; (search-for-primes 1000000000000000 1000000000000100)
; (search-for-primes 10000000000000000 10000000000000100)
; (search-for-primes 100000000000000000 100000000000000100)
; (search-for-primes 1000000000000000000 1000000000000000100)
; (search-for-primes 10000000000000000000 10000000000000000100)
; (search-for-primes 100000000000000000000 100000000000000000100)
; (search-for-primes 1000000000000000000000 1000000000000000000100)
; (search-for-primes 10000000000000000000000 10000000000000000000100)
; (search-for-primes 100000000000000000000000 100000000000000000000200)
; (search-for-primes 1000000000000000000000000 1000000000000000000000200)
; (search-for-primes 10000000000000000000000000 10000000000000000000000300)
; (search-for-primes 100000000000000000000000000 100000000000000000000000300)
; (search-for-primes 1000000000000000000000000000 1000000000000000000000000300)
; (search-for-primes 10000000000000000000000000000 10000000000000000000000000500)
; (search-for-primes 100000000000000000000000000000 100000000000000000000000000500)

; With the logarithmic runtime even very big numbers are computed very quickly
; and no data can be obtained.
