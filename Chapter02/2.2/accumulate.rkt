#lang sicp

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op
                                initial
                                (cdr seq)))))

(define (length sequence)
  (accumulate (lambda(x y)(if (null? y)
                            0 
                            (+ 1 (length (cdr sequence)))))
              0 sequence))

;(accumulate + 1 (list 1 2 3 4 5))
;(length (list 1 2 3 4 5 6 7 8 9))

;; evaluates a polynomial using Horner’s rule. 
(define
  ;(horner-eval x coefficient-sequence)
  ;(accumulate
   ;(lambda (this-coeff higher-terms)
     ;(+ (* higher-terms x) this-coeff))
   ;0
   ;coefficient-sequence))

;(horner-eval 2 (list 1 3 0 5 0 1))
