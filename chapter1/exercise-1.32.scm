; Exercise 1.32
; a. Show that sum and product (Exercise 1.31) are both special cases of a
;    still more general notion called accumulate that combines collection of
;    terms, using some general accumulation function:
;
;    (accumulate combiner null-value term a next b)
;
;    accumulate takes as arguments the same term and range specifications as
;    sum and product, together with a combiner procedure (of two arguments)
;    that specifies what base value to use when the terms run out. Write
;    accumulate and show how sum and product can both be defined as simple
;    calls to accumulate.
;
; b. If your accumulate procedure generates a recursive process, write one that
;    generates an iterative process. If it generates as iterative process,
;    write one that generates a recursive process.

(define (accumulate combiner null-value term a next b)
        (if (> a b)
            null-value
            (combiner (term a)
                      (accumulate combiner
                                  null-value
                                  term
                                  (next a)
                                  next
                                  b))))

(define (sum term a next b)
        (accumulate + 0 term a next b))

(define (product term a next b)
        (accumulate * 1 term a next b))

; Test functions
(define (sum-integers a b)
        (define (identity x) x)
        (sum identity a 1+ b))

(define (sum-cubes a b)
        (define (cube x) (* x x x))
        (sum cube a 1+ b))

(define (factorial x)
        (define (identity a) a)
        (product identity 1 1+ x))

; Test cases
(sum-integers 1 10)
(sum-cubes 1 10)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

(define (accumulate combiner null-value term a next b)
        (define (iter a result)
                (if (> a b)
                    result
                    (iter (next a)
                          (combiner (term a)
                                    result))))
        (iter a null-value))

(sum-integers 1 10)
(sum-cubes 1 10)
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
