; Exercise 1.4
; Observe that our model of evaluation allows for combinations whose operators
; are compound expressions. Use this observation to describe the behaviour of
; the following procedure.

(define (a-plus-abs-b a b)
        ((if (> b 0) + -) a b))

; The procedure uses define, which is a special form, to define a new procedue
; "a-plus-abs-b" as the sum of a and b where a and b are the 2 formal
; parameters. But the sum expression is interesting because instead of simply
; using the primitive "+" operator, it uses a combination. The combination
; happens to be a special form "if" which checks whether b is greater then 0 or
; not. If it is, then it returns "+" otherwise "-" which correspondingly adds
; or substracts b from a, hence effectively adding absolute value of b to a.
