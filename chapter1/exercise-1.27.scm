; Exercise 1.27
; Demonstrate that the Carmichael numbers listed in Footnote 1.47 really do
; fool the Fermat test. That is, write a procedure that takes an integer n and
; tests whether a^n is congruent to a modulo n for every a < n, and try your
; procedure on the given Carmichael numbers.

(define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder (square (expmod base (/ exp 2) m))
                          m))
              (else (remainder (* base
                                  (expmod base (- exp 1) m))
                               m))))


(define (carmichael-test n)
        (define (test a)
                (= (expmod a n n) a))
        (define (iter a)
                (cond ((= n a) true)
                      (else (if (test a)
                                (iter (1+ a))
                                false))))
        (iter 1))

; Test cases

(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)

(carmichael-test 562)
(carmichael-test 1899)
