; TYPE YOUR NAME HERE: Ashraq Khan

; Here is the second quiz from the summer class, offered here as a practice problem

;; Develop, write and certify a properly recursive procedure reverseDigits to input a positive integer n
;; and to output the integer formed by reversing the digits of n.  Thus (reverseDigits 1234) returns the
;; integer 4321.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PRECONDITION: n > 0
; POSTCONDITION: digits of positive integer i are reversed

; Let's first confirm some pre-reqs.
; reverseDigits is properly recursive, as it is guarded (note that it's being added
; after every call).
; Note that n decreases every round via the first-dig-popper function, which removes
; the first digit of any positive integer if it is greater than or equal to 10, or returns
; 0 otherwise.

; We can now construct a certification on n by performing induction on n. For all positive
; integers n (n > 0), the value of (reverseDigits n) is the digits of n reversed. In other
; words, if n were to be represented as:

; n_i * 10^i + n_i-1 * 10^i-1 + ... + n_0 * 10^0

; where i is the number of digits minus 1, then (reverseDigits n) will result in:

; n_0 * 10^i + n_1 * 10^i-1 + ... + n_i * 10^0

; BASIS STEP: When n < 10, then (reverseDigits n) = n, which makes sense. 

; INDUCTION HYPOTHESIS: Assume that the recursion call returns the correct value, which is
; the number n with its most significant digit removed and order is reversed, or:

; n_0 * 10^i-1 + n_1 * 10^i-2 + ... + n_i-2 + 10^1 + n_i-1 + 10^0

; INDUCTION STEP: Show that (reverseDigits n) = integer with digits of n reversed when
; n > 0. Upon calling the function, we see that if n > 10,

; (reverseDigits n) = (+ (reducer n) (* 10 (reverseDigits (first-dig-popper n))))

; (reducer n) returns the first digit of n, which we will write as n_i.
; By induction hypothesis, we can rewrite this as:

; (reverseDigits n) = (+ n_i (* 10 (n_0 * 10^i-1 + n_1 * 10^i-2 + ... + n_i-2 + 10^1 + n_i-1 + 10^0)))
;                   = (+ n_i (n_0 * 10^i + n_1 * 10^i-1 + ... + n_i-2 * 10^2 + n_i-1 * 10^1))
;                   = n_0 * 10^i + n_1 * 10^i-1 + ... + n_i

; Note: we simplified n_i * 10^0 to n_i, since 10^0 = 1 and any number times 1 is the number itself.

(define (reverseDigits n)
  (cond ((< n 10) n)
        (else (+ (reducer n)
                 (* 10 (reverseDigits (first-dig-popper n)))))))

(define (reducer n)
  (cond ((< n 10) n)
        (else (reducer (floor (/ n 10))))))

(define (first-dig-popper n)
  (cond ((< n 10) 0)
        (else (+ (modulo n 10)
                 (* 10 (first-dig-popper (floor (/ n 10))))))))

(reverseDigits 123456) ; returns 654321











