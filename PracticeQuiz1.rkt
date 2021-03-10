; Practice Quiz Problem

; Develop and prove an iterative scheme function DigitSpread of one integer
; argument n which returns the difference between the largest and smallest
; digits occuring in n.

; Time Now: 4:10pm
; Time when finished: 4:54pm
; Time Stop: 4:50pm

; ------------ Iterative Process ------------
; PRECONDITION: n is a nonnegative integer
; POSTCONDITION: result is difference between largest and smallest integer

; We want to utilize each digit of the integer and build up to our desired
; result, which is the difference.

; Let's come up with a typical call:
; (number result min max)
; (12345 0 9 0)
; (1234 0 5 5) We say that the current max and min numbers are 5
; (123 1 4 5) Check if 4 is less than 5, if it is, we have new min.
;             If it's bigger, we have new max.
; (12 2 3 5)
; (1 3 2 5)
; (0 4 1 5) Returns 4 when n = 0

; What are the design roles for our variables?
; number = number from which we extract our biggest/smallest digits from,
;          decreases by factor of 10 until it is 0
; result = the current difference between max and min digit
; min = smallest digit so far
; max = biggest digit so far

; Let's see if we can come up with some sort of guess invariant.
; number = (quotient number 10)
; result = max - min
; max = biggest digit so far from number
; min = smallest digit so far from number

; --- Strong Enough?
; Check last call, when number = 0. At that point, the procedure will return
; result, which is:
; result = max - min, where max is the biggest number so far and min is the
;                     smallest number so far.
; So far, we've traversed the entire number, so result is what we want and the
; GI holds.

; --- Weak Enough?
; Check first call, when number = n_0 * 10^0 + n_1 * 10^1 + n_2 * 10^2 + ... + n_k * 10^k,
; where k is the position of the first number.
; The function will check the last digit and see if it is smaller than the min or larger than
; the max. Since min starts at 9 and max starts at 0, the last digit will automatically be
; equal to both the min and max, thus making the result, or difference, 0. The GI holds.

; --- Preservability?
; Check the kth call, when number = n_0 * 10^0 + n_1 * 10^1 + n_2 * 10^2 + ... + n_k * 10^k,
; where k is the position of the last digit.
; After completing the call, number = (quotient number 10), which is:
; n_0 * 10^0 + n_1 * 10^1 + n_2 * 10^2 + ... + n_k-1 * 10^k-1
; The function will check if n_k is bigger than the current max or smaller than the current
; min and if it is, it replaces it.
; This will increase the margin of difference from max to min, which means that the value of
; result will either stay the same or increase. Thus, the GI holds.

; --- Termination?
; Upon every call, the function reduces the size of number by a factor of 10, since
; number = (quotient number 10)
; We terminate when number = 0, and since the precondition states that number is nonnegative,
; we will eventually reach the termination case of number = 0. We can conclude that this
; program will terminate.

; --- Code ---

(define (DigitSpread n)
  (define (iter n result small big)
    (if (= n 0)
        result
        (let ((b (max (remainder n 10) big)) (s (min (remainder n 10) small)))
          (iter (quotient n 10) (- b s) s b))))
  (iter (abs n) 0 9 0))

; Test Cases

(DigitSpread 12345) ; 4
(DigitSpread 0)     ; 0
(DigitSpread 1)     ; 0
(DigitSpread 54321) ; 4
(DigitSpread 98789) ; 2




