; TYPE YOUR NAME HERE: Ashraq Khan

; Students had 45 minutes for the first quiz presented in the Summer term.

;; The primitive function list takes a finite number of numbers (say x1, x2, ..., xk) and returns the list
;; (x1 x2 ... xk) of these numbers in the same order.  Thus if x1 = 10, x2 = 3, x3 = 6, and x4 = 20, the call
;;   (list x1 x2 x3 x4)
;; returns
;;   (10 3 6 20)

;; In addition, the primitive function cons takes two arguments -- a number new, and a list lst, and
;; returns the list formed by inserting new at the front of lst.  Thus (cons 10 (list 3 6 20)) returns
;; (10 3 6 20).  


;; Write a function sortFive to input 5 distinct integers and return the list of the input values, sorted
;; from smallest to largest.  Thus (sortFive 10 5 20 0 6) returns (0 5 6 10 20).

;; You may NOT use the primitives min or max, or any kind of recursion or iteration, either in sortFive itself
;; or in any helper functions you define.  

;; You MUST use cond, let, cons and list.

;; (HINT: you DO want to define helper functions.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; INSERT YOUR ANSWER HERE

; take a list of 5 integers and create ordered list
; check what is smallest without using min/max
; have helper function determine what is smallest, place as first in list

(define (sort-5 x1 x2 x3 x4 x5)
  (let ((m (min-5 x1 x2 x3 x4 x5)))
    (cond ((= m x1) (cons x1 (sort-4 x2 x3 x4 x5)))
          ((= m x2) (cons x2 (sort-4 x1 x3 x4 x5)))
          ((= m x3) (cons x3 (sort-4 x1 x2 x4 x5)))
          ((= m x4) (cons x4 (sort-4 x1 x2 x3 x5)))
          (else     (cons x5 (sort-4 x1 x2 x3 x4))))))

(define (sort-4 x1 x2 x3 x4)
  (let ((m (min-4 x1 x2 x3 x4)))
    (cond ((= m x1) (cons x1 (sort-3 x2 x3 x4)))
          ((= m x2) (cons x2 (sort-3 x1 x3 x4)))
          ((= m x3) (cons x3 (sort-3 x1 x2 x4)))
          (else     (cons x4 (sort-3 x1 x2 x3))))))

(define (sort-3 x1 x2 x3)
  (let ((m (min-3 x1 x2 x3)))
    (cond ((= m x1) (cons x1 (sort-2 x2 x3)))
          ((= m x2) (cons x2 (sort-2 x1 x3)))
          (else     (cons x3 (sort-2 x1 x2))))))

(define (sort-2 x1 x2)
  (cond ((> x1 x2) (list x2 x1))
        (else     (list x1 x2))))

(define (min-5 x1 x2 x3 x4 x5)
  (let ((m (min-4 x2 x3 x4 x5)))
    (cond ((> x1 m) m)
          (else x1))))

(define (min-4 x1 x2 x3 x4)
  (let ((m (min-3 x2 x3 x4)))
    (cond ((> x1 m) m)
          (else x1))))

(define (min-3 x1 x2 x3)
  (let ((m (min-2 x2 x3)))
    (cond ((> x1 m) m)
          (else x1))))

(define (min-2 x1 x2)
  (cond ((> x1 x2) x2)
        (else x1)))
    
(sort-5 9 2 3 4 10)








