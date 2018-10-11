; Exercise 1.35
; Show that the golden ratio φ (Section 1.2.2) is a fixed point of the
; transformation x -> 1 + 1/x, and use this fact to compute φ by means of the
; fixed-point procedure.

(define tolerance 0.00001)

(define (close-enough? a b)
        (< (abs (- a b)) tolerance))

(define (average a b)
        (/ (+ a b)
           2))

(define (fixed-point f initial-guess)
        (define (try guess)
                (let ((next (f guess)))
                     (if (close-enough? guess next)
                         next
                         (try next))))
        (try initial-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
