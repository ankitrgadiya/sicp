; Exercise 1.11
; A function f is defined by the rule that
; f(n) = n if n < 3
;        f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3
;
; Write a procedure that computes f by means of a recursive process. Write
; a procedure that computes f by means of an iterative process.

; Recursive
(define (f-recr n)
        (if (< n 3)
            n
            (+ (f-recr (- n 1))
               (* 2 (f-recr (- n 2)))
               (* 3 (f-recr (- n 3))))))

; Test cases
(f-recr 1)
(f-recr 2)
(f-recr 3)
(f-recr 4)
(f-recr 5)
(f-recr 6)

; Iterative
(define (f-iter n)
        (define (iter a b c count)
                (cond ((= count 1) 1)
                      ((= count 2) b)
                      (else (iter (+ a
                                     (* 2 b)
                                     (* 3 c))
                                  a
                                  b
                                  (- count 1)))))
        (iter 4 2 1 n))

; Test cases
(f-iter 1)
(f-iter 2)
(f-iter 3)
(f-iter 4)
(f-iter 5)
(f-iter 6)
