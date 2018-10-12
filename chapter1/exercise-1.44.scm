; Exercise 1.44
; The idea of smoothing a function is an important concept in signal
; processing. If f is a function and dx is some small number, then the smoothed
; version of f is the function whose value at a point x is the average of f(x -
; dx), f(x), and f(x + dx). Write a procedure smooth that takes as input a
; procedure that computes f and returns a procedure that computes the smoothed
; f. It is sometimes valuable to repeatedly smooth a function (that is, smooth
; the smoothed functionm, and so on) to obtain the n-fold smoothed function.
; Show how to generated the n-fold smoothed function of any given function
; using smooth and repeated from Exercise 1.43.

(define (repeated f n)
        (define (iter n result)
                (if (= n 0)
                    result
                    (iter (-1+ n)
                          (f result))))
        (lambda (x) (iter n x)))

(define (smooth f)
        (define dx 0.00001)
        (lambda (x) (/ (+ (f (- x dx))
                          (f x)
                          (f (+ x dx)))
                       3)))

(define (smooth-repeat f n)
        ((repeated smooth n) f))
