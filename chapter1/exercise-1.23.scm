; Exercise 1.23
; The smallest-divisor procedure shown at the start of this section does lots
; of needless testing: After it checks to see if the number is divisible by 2
; there is no point in checking to see if it is divisible by any larger even
; numbers. This suggests that the values used for test-divisor should not be 2,
; 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, ...
; To implement this change, define a procedure next that returns 3 if its input
; is equal to 2 and otherwise returns its input plus 2. Modify the
; smallest-divisor procedure to use (next test-divisor) instead of (+
; test-divisor 1). With timed-prime-test incorporating this modified version of
; smallest-divisor, run the test for each of the 12 primes found in Exercise
; 1.22. Since this modification halves the number of test steps, you should
; expect it to run about twice as fast. Is this expectation confirmed?  If not,
; what is the observed ratio of the speeds of the two algorithms, and how do
; you explain the fact that it is different from 2?

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

(define (prime? n)
        (define (divides? a b) (= (remainder b a) 0))
        (define (find-divisor test-divisor)
                (cond ((> (square test-divisor) n) n)
                      ((divides? test-divisor n) test-divisor)
                      (else (find-divisor (next test-divisor)))))
        (define (smallest-divisor n) (find-divisor  2))
        (= (smallest-divisor n) n))

(define (search-for-primes a b)
        (define (search-iter a)
                (if (< a b) (timed-prime-test a))
                (if (< a b) (search-iter (+ a 2))))
        (if (= (remainder a 2) 0)
            (search-iter (1+ a))
            (search-iter a)))

(define (next a)
        (if (= a 2)
            3
            (+ a 2)))

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
; 1000000007 0.016 (ratio 1.875)
; 1000000009 0.02 (ratio 1.15)
; 1000000021 0.026 (ratio (1.269)
(search-for-primes 10000000000 10000000062)
; 10000000019 0.063 (ratio 1.4285)
; 10000000033 0.073 (ratio 1.2739)
; 10000000061 0.063 (ratio 1.476)
(search-for-primes 100000000000 100000000058)
; 100000000003 0.18 (ratio 1.55)
; 100000000019 0.18 (ratio 1.55)
; 100000000057 0.18 (ratio 1.55)
(search-for-primes 1000000000000 1000000000064)
; 1000000000039 0.57 (ratio 1.6140)
; 1000000000061 0.56 (ratio 1.6964)
; 1000000000063 0.58 (ratio 1.5517)

; The average of all the observed ratio is about 1.4 which is not equal to 2.
; This might be because we did reduce the total test-divisor to half but now
; for each test-divisor additional procedure needs to be called which in turn
; runs an if conditional.
