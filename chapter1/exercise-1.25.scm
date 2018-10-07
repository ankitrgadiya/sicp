; Exercise 1.25
; Alyssa P. Hacker complains that we went to a lot of extra work in writing
; expmod. After all, she says, since we already know how to compute
; exponentials, we could have simple written

(define (expmod base exp m)
        (remainder (fast-expt base exp) m))

; Is she correct? Would this procedure serve as well for our fast prime tester?
; Explain.

(define (fast-expt b n)
        (define (even? x)
                (= (remainder x 2) 0))
        (define (expt-iter b n a)
                (cond ((= n 0) a)
                      ((even? n) (expt-iter (* b b) (/ n 2) a))
                      (else (expt-iter b (- n 1) (* a b)))))
        (expt-iter b n 1))

(define (even? x) (= (remainder x 2) 0))
(define (fermat-test n)
        (define (try-it a)
                (= (expmod a n n) a))
        (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
        (cond ((= times 0) true)
              ((fermat-test n) (fast-prime? n (- times 1)))
              (else false)))

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

(define (search-for-primes a b)
        (define (search-iter a)
                (if (< a b) (timed-prime-test a))
                (if (< a b) (search-iter (+ a 2))))
        (if (= (remainder a 2) 0)
            (search-iter (1+ a))
            (search-iter a)))

(search-for-primes 1000 1020)
(search-for-primes 10000 10038)
(search-for-primes 100000 100044)
(search-for-primes 1000000 1000038)

; fast-expt takes significantly more time them the earlier more efficient
; expmod because with fast-expt we are calculating the actual exponential which
; gets very big very fast and even though it is taking logarithmic time, still
; with very very big numbers it takes significantly longer to run. However, in
; earlier expmod we are not calculating actual exponential but we are reducing
; the exponent to 1 and then squaring the remainder successfully so on each
; step we get base comparable to m itself which can be computed very quickly
; relative to the very very big numbers generated using fast-expt.

; In practice it took very long to calculate. Even relatively smaller numbers
; in the range of 100000 took minutes to calculate whereas using the earlier
; expmod it took no time to calculate these numbers.
