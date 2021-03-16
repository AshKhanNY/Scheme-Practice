; This works by manipulating the last three digits of an integer and placing the
; biggest digit so far in the last digit place and the
; smallest digit so far in the second to last digit place.

(define (spreadDigit n)
  (if (< n 100)
      ; IF TRUE: calculates the absolute value of the difference between 
      ; the second to last digit and the last digit
      (abs (- (modulo n 10)
              (modulo (quotient n 10) 10)))
      ; ELSE: swaps the last digit and the second to last digit
      ; if the second to last digit is greater than the last digit
      (let ((m (if (> (modulo (quotient n 10) 10) (modulo n 10))
                   (+ (* (quotient n 100) 100)
                      (* (modulo n 10) 10)
                      (modulo (quotient n 10) 10))
                   n)))
        ; performs digit swaps whenever the third to last digit is
        ; smaller than the second to last digit (smallest so far) or
        ; bigger than the last digit (biggest so far). 
        (cond ((< (modulo (quotient m 100) 10) (modulo (quotient m 10) 10))
               (spreadDigit (+ (* (quotient m 100) 10)   (modulo m 10))))
              
              ((> (modulo (quotient m 100) 10) (modulo m 10))
               (spreadDigit (+ (* (quotient m 1000) 100)
                               (* (modulo (quotient m 10) 10) 10)
                               (modulo (quotient m 100) 10))))
              
              ; Loses third to last digit if it is equal to either the smallest or biggest so far
              (else
               (spreadDigit (+ (* (quotient m 1000) 100)
                               (* (modulo (quotient m 10) 10) 10)
                               (modulo m 10))))))))

; Test Cases

(spreadDigit 6517)     ; 6
(spreadDigit 990)      ; 9
(spreadDigit 9757434)  ; 6
(spreadDigit 83827)    ; 6
(spreadDigit 0)        ; 0
(spreadDigit 8)        ; 8
(spreadDigit 91)       ; 8
