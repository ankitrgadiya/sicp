; Exercise 1.12
; The following pattern of numbers is called Pascal's triangle.
;     1
;   1 2 1
;  1 3 3 1
; 1 4 6 4 1
;    ...
; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes elements of Pascal's triangle by means of a recursive process.

(define (pascal row col)
        (cond ((< row col) 0)
              ((= row 0) 1)
              ((= col 0) 1)
              ((= row col) 1)
              (else (+ (pascal (-1+ row) (-1+ col))
                       (pascal (-1+ row) col)))))

; Test cases
(pascal 3 2)
(pascal 5 4)
(pascal 6 3)
