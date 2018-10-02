; Exercise 1.18
; Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
; generates an iterative process for multiplying two integers in terms of
; adding, doubling, and halving and uses a logarithmic number of steps.

(define (* a b)
        (define (halve x) (/ x 2))
        (define (double x) (+ x x))
        (define (even? x) (= (remainder x 2) 0))
        (define (mult-iter a b c)
                (cond ((= b 0) c)
                      ((even? b) (mult-iter (double a)
                                            (halve b)
                                            c))
                      (else (mult-iter a (- b 1) (+ c a)))))
        (mult-iter a b 0))

; Test cases
(* 2 0)
(* 0 4)
(* 2 3)
(* 4 10)
