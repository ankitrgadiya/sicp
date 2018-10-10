; Exercise 1.34
; Suppose we define the procedure

(define (f g) (g 2))

; Then we have

(f square)

(f (lambda (z) (* z (+ z 1))))

; What happens if we (perversely) ask the interpreter to evaluate the
; combination (f f)? Explain.

(f f)

; Using the substitution method:
; (f f)
; (f 2)
; (2 2)
; We finally get (2 2) which means we call 2 procedure and apply it to 2. But 2
; is not actually a procedure and so interpreter will throw an error saying 2
; is not applicable.
