#lang sicp
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (fold-right op
                                initial
                                (cdr seq)))))

(define (map-tree op tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (op tree))
        (else
         (cons (map-tree op (car tree)) (map-tree op (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
