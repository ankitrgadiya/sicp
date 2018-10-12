; Exercise 1.40
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

(define tolerance 0.00001)

(define (close-enough? a b)
        (< (abs (- a b)) tolerance))

(define (fixed-point f initial-guess)
        (define (try guess)
                (let ((next (f guess)))
                     (if (close-enough? guess next)
                         next
                         (try next))))
        (try initial-guess))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define dx 0.00001)

(define (deriv g)
        (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newtons-transform g)
        (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
        (fixed-point (newtons-transform g) guess))

(define (cubic a b c)
        (lambda (x)
                (+ (cube x)
                   (* a (square x))
                   (* b x)
                   c)))

(newtons-method (cubic 3 -11 -6) 1)
