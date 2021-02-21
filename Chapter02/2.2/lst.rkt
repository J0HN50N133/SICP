#lang sicp
(define (length lst)
  (if (null? lst) 0 (+ 1 (length (cdr lst)))))
(length (list 1 2 3 4))
