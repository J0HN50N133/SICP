#lang sicp
(define (enumerate tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate (car tree)) (enumerate (cdr tree))))))
                               
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op
                                initial
                                (cdr seq)))))
