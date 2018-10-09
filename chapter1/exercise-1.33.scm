; Exercise 1.33
; You can obtain an even more general version of accumulate (Exercise 1.32) by
; introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes the
; same arguments as accumulate, together with an additional predicate of one
; argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:
;
; a. the sum of the squares of the prime numbers in the interval a to b
;    (assuming that you have a prime? predicate already written)
;
; b. the product of all the positive integers less than n that are relatively
;    prime to n (i.e., all positive integers i < n such that GCD(i, n) = 1).

(define (filtered-accumulate predicate? combiner null-value term a next b)
        (define (filter x)
                (if (predicate? x)
                    (combiner (term a)
                              (filtered-accumulate predicate?
                                                   combiner
                                                   null-value
                                                   term
                                                   (next a)
                                                   next
                                                   b))
                    (filtered-accumulate predicate?
                                         combiner
                                         null-value
                                         term
                                         (next a)
                                         next
                                         b)))
        (if (> a b)
            null-value
            (filter a)))

(define (sum-square-prime a b)
        (define (prime? n)
                (define (divides? a b) (= (remainder b a) 0))
                (define (find-divisor test-divisor)
                        (cond ((> (square test-divisor) n) n)
                              ((divides? test-divisor n) test-divisor)
                              (else (find-divisor (1+ test-divisor)))))
                (define (smallest-divisor n) (find-divisor  2))
                (= (smallest-divisor n) n))
        (filtered-accumulate prime? + 0 square a 1+ b))

(sum-square-prime 1 10)

(define (co-prime-product n)
        (define (gcd-filter x)
                (= 1 (gcd x n)))
        (define (identity x) x)
        (filtered-accumulate gcd-filter
                             *
                             1
                             identity
                             1
                             1+
                             (- n 1)))

(co-prime-product 10)
