; Exercise 1.22
; Most Lisp implementations include a primitive called runtime that returns an
; integer that specifies the amount of time the system has been running
; (measured, for example, in microseconds). The following timed-prime-test
; procedure, when called with an integer n, prints n and checks to see if n is
; prime. If n is prime, the procedure prints three asterisks followed by the
; amount of time used in performing the test.

(define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
        (if (prime? n)
            (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time))

; Using this procdure, write a procedure search-for-primes that checks the
; primality of the consecutive odd integers in a specified range. Use your
; procedure to find the three smallest primes larger than 1000; larger than
; 10,000; larger than 100,000; larger that 1,000,000. Note the time needed to
; test each prime. Since the testing algorithm has order of growth of Θ(√n),
; you should expect that testing for primes around 10,000 should take about √10
; times as long as testing for primes around 1000. Do your timing data bear
; this out? How well do the data for 100,000 and 1,000,000 support the Θ(√n)
; prediction? Is your result compatible with the notion that programs on your
; machinie run in time proportional to the number of steps required for the
; computation?

; Required procedure
(define (prime? n)
        (define (divides? a b) (= (remainder b a) 0))
        (define (find-divisor test-divisor)
                (cond ((> (square test-divisor) n) n)
                      ((divides? test-divisor n) test-divisor)
                      (else (find-divisor (1+ test-divisor)))))
        (define (smallest-divisor n) (find-divisor  2))
        (= (smallest-divisor n) n))

(define (search-for-primes a b)
        (define (search-iter a)
                (if (< a b) (timed-prime-test a))
                (if (< a b) (search-iter (+ a 2))))
        (if (= (remainder a 2) 0)
            (search-iter (1+ a))
            (search-iter a)))

; (search-for-primes 1000 1020)
; 1009
; 1013
; 1019
; (search-for-primes 10000 10038)
; 10007
; 10009
; 10037
; (search-for-primes 100000 100044)
; 100003
; 100019
; 100043
; (search-for-primes 1000000 1000038)
; 1000003
; 1000033
; 1000037

; For the values given in question, the computer runs the procedure so fast
; and no data can be obtained. So, I went to higher and higher values to get
; some data.

; (search-for-primes 10000000 10000104)
; 10000019
; 10000079
; 10000103
; (search-for-primes 100000000 100000040)
; 100000007
; 100000037
; 100000039

(search-for-primes 1000000000 1000000022)
; 1000000007 0.03
; 1000000009 0.03
; 1000000021 0.03
(search-for-primes 10000000000 10000000062)
; 10000000019 0.09
; 10000000033 0.093
; 10000000061 0.093
(search-for-primes 100000000000 100000000058)
; 100000000003 0.28
; 100000000019 0.28
; 100000000057 0.28
(search-for-primes 1000000000000 1000000000064)
; 1000000000039 0.92
; 1000000000061 0.95
; 1000000000063 0.90

; The results satisfy the prediction.
; 0.03 * √10 = 0.09486
; 0.09 * √10 = 0.2846
; 0.28 * √10 = 0.8854
