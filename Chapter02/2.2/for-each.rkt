#lang sicp
(define (for-each op sequence)
  (if (not (null? sequence)) (op (car sequence)))
  (if (not (null? sequence)) (for-each op (cdr sequence))))

(for-each (lambda(x)(newline)(display x)) (list 57 321 88 70))
