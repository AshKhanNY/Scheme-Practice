
; Third Homework Set
; CSc 335
; Spring 2021


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; based on lecture5.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Review the lecture notes, and read the associated
; sections in Abelson and Sussman, before starting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Proofs are required for all programs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is another long problem set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Abelson and Sussman, Problems 1.29, 1.30, 1.31, 1.34, and 1.37

; Abelson and Sussman, Problems 1.41, 1.42, and 1.43


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.29
; Simpson's Rule Integral
; PRECONDITION: Function f is a valid lambda expression, n is a non-negative integer
; POSTCONDITION: Integral of a function from a to b is returned

; Equations:
; h = (b - a) / n
; y_k = f (a + kh)

; We want to update y in every step such that it follows pattern of 4, 2, 4, ... 4, 2, 4
; Change the "next" function so that it looks for this pattern
; If y is first or last term, coefficient is 1
; If y is odd, coefficient is 4
; If y is even, coefficient is 2
; Make sure to multiply the sum of the integral with h/3

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (skip-two y)
      (+ y (* 2 h)))
    (+ (* (/ h 3) (+ (f a) (f (+ a (* n h)))))
       (* (/ (* 4 h) 3) (sum f (+ a (* 1 h)) skip-two (- b (+ a h))))
       (* (/ (* 2 h) 3) (sum f (+ a (* 2 h)) skip-two (- b (+ a (* 2 h))))))))

(define (cube x) (* x x x))

(simpson-integral cube 0 1 100)


; Exercise 1.30
; Iterative Summation
; PRECONDITION: a and b are non-negative integers, term is a valid expression for one variable,
;               result of next is greater than the input for the function
; POSTCONDITION: returns sum of all f(n), a <= n <= b

; We want to build up to our solution, so we can keep track of what has been
; processed so far and what needs to be processed still
; Sum = sum calculated so far + sum remaining
; Sum = 0 when a > b
;     = summation of f(n) when a <= b

; Let's try an example: sum of squares
; We should get 1^2 + 2^2 + 3^2 + 4^2 + 5^2 = 1+4+9+16+25 = 55

; (sum term a next b)
; (iter a result)
; term = x^2, a = 1, b = 5, next = x (identity)
; (1 0)
; (2 1)
; (3 5)
; (4 14)
; (5 30)
; (6 55)

; We can assign some design roles for our variables now:
; a = current term in summation
; result = result of the sum so far

; How does this program change per call?
; a --> (next a)
; result --> result + (term a)

; TEST THE GI:
; Assume A is the inital a provided by user.

; Strong Enough? - Last Call
; When a = (next a), where (next a) > b,
;      result = (term (next a)) + sum of term of numbers A to (next a-1)
; On the last call, a = (next a), and (next a) > b, so the result, which is
; (term (next a)) + sum of terms of A to (next (a-1)), would be returned and the GI would hold.
 
; Weak Enough? - First Call
; When a = A, result is (term a), which makes sense because the sum of the term of one
; number is the term of the number itself. The GI holds.

; Preservability? - Consecutive Calls
; Assume on the kth call, the GI holds - a = k, result = (term k) + sum of terms of A to k-1
; On the (next k)th call, we have:
; a = (next k), result = (term (next k)) + (term k) + sum of terms of A to k-1
;                 = (term k+1) + sum of terms of A to k
; As we can see, the GI holds.

; Termination
; Function terminates when a > b. Our precondition states that next yields a value that is
; greater than the number inputted to the next function. Thus, the value of a increases
; after every iterative call because of (next a), and eventually a will be greater than b.
; Thus, this terminates.

; Code:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (square x) (* x x))

(sum square 1 (lambda (x) (+ x 1)) 5) ; sum of squares from 1 to 5, returns 55

; Exercise 1.31
; Product Function w/ Proofs

; ------------ Recursive ------------
; Given a term, lower bound a, upper bound b, and a next function that increases value
; of a after every call, let's come up with a product function that recursively computes
; the product of all terms from domain a to b.

; Code
(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (inc x) (+ x 1))
(define (identity x) x)

(product identity 1 inc 3) ; returns 6

; Let's start with the induction:
; BASE CASE
; If a > b, we terminate and return... what? 0 or 1?
; If it is 0, then it would be multiplied to all terms where a <= b, giving us a 0 which
; is not what we want. Let's stick to 1.

; INDUCTION HYPOTHESIS
; Assume the recursive call returns the correct value, so:
; (product term (next a) next b) --> product of all terms from (next a) to b

; INDUCTION STEP

; ------------ Iterative ------------
; PRECONDITION: 


; Exercise 1.34

(define (f g)
  (g 2))

(f f) ; On second call of f, you cannot do (2 2) because "2" is not a procedure


; Exercise 1.37



; Exercise 1.41

; Exercise 1.42

; Exercise 1.43

