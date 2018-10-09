; Exercise 1.31
; a. The sum procedure is only the simplest of a vast number of similar
;    abstractions that can be captured as higher-order procedures. Write an
;    analogous procedure called product that returns the product of the values
;    of a function at points over a given range. Show how to define factorial
;    in terms of product. Also use product to compute approximations to π using
;    the formula
;     π/4 = (2·4·4·6·6·8...)/(3·3·5·5·7·7·...)
;
; b. If your product procedure generates a recursive process, write one that
;    generates an iterative process. If it generates an iterative process,
;    write one that generates a recursive process.

(define (product-recr a b term next)
        (if (> a b)
            1
            (* (term a)
               (product-recr (next a) b term next))))

(define (factorial x)
        (define (identity a) a)
        (product-recr 1 x identity 1+))

; Test cases
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)

(define (approx-pi n)
        (define (term a)
                (/ (square a)
                   (* (- a 1) (+ a 1))))
        (define (next a) (+ 2 a))
        (* 4
           (/ 2.0 3.0)
           (product-recr 4 n term next)))

(approx-pi 20000)


(define (product-iter a b term next)
        (define (iter a result)
                (if (> a b)
                    result
                    (iter (next a)
                          (* result
                             (term a)))))
        (iter a 1))

; Test cases
(define (factorial x)
        (define (identity a) a)
        (product-iter 1 x identity 1+))

(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)
(factorial 5)
