; Second Homework Set
; CSc 335
; Spring 2021


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is a long problem set - you will want to set aside some hours.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I am posting this somewhat in advance of discussing
; the relevant material in lecture.  As I have said, it is a good idea
; to start thinking about the problems right away: you'll be primed to
; pick up on information which will help you solve them.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proofs must be given for all programs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; 1.  Abelson and Sussman, Problems 1.11 and 1.12

; 2.  Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.

; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

; You may find the built-in functions quotient, remainder, truncate, zero? -- and perhaps others --
; helpful as you design your solutions for problems 2 and 3.  Have a look at the Scheme
; manual.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.11

; Recursive Process
(define (func-r n)
  (if (< n 3) n
      (+ (func-r (- n 1))
         (* 2 (func-r (- n 2)))
         (* 3 (func-r (- n 3))))))

(func-r 5) ; 25

; Iterative Process

(define (func-i n)
  (func-iter 2 1 0 n))

(define (func-iter a b c count)
  (if (= count 0) c
      (func-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(func-i 5) ; 25

; Exercise 1.12

(define (pascal row col)
  (cond ((= row 1) 1)
        ((or (= col 1) (= col row)) 1)
        ((> col row) 0)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

(pascal 7 4) ; 20

; 2.  Write iterative and recursive scheme functions to return the sum of the digits within
; a non-negative integer.  For example, (sum-of-digits 345) is 12.

; Recursive

(define (sum-of-digits n)
  (cond ((< n 10) n)
        (else (+ (modulo n 10) (sum-of-digits (quotient n 10))))))

(sum-of-digits 345) ; 12
 
; Iterative

(define (sum-of-digits n)
  (sum-of-digits-iter 0 n))

(define (sum-of-digits-iter sum n)
  (if (< n 10)
      (+ sum n)
      (sum-of-digits-iter (+ sum (modulo n 10)) (quotient n 10))))

(sum-of-digits 345) ; 12

; 3.  Write iterative and recursive scheme programs to test whether the digits in a non-negative
; integer are in increasing order.  For example, the digits of 12348 are in increasing order, while
; those of 12343 are not.

; Recursive

(define (ordered-digits n)
  (cond ((zero? n) #t)
        (else (and (>= (modulo n 10)
                       (modulo (quotient n 10) 10))
                   (ordered-digits (quotient n 10))))))

(ordered-digits 12348) ; #t

(ordered-digits 12343) ; #f

; Iterative

(define (ordered-digits n)
  (ordered-digits-iter (modulo n 10) n))

(define (ordered-digits-iter curr n)
  (cond ((zero? n) #t)
        ((< curr (modulo (quotient n 10) 10)) #f)
        (else (ordered-digits-iter (modulo (quotient n 10) 10) (quotient n 10)))))

(ordered-digits 12348) ; #t

(ordered-digits 12343) ; #f

















