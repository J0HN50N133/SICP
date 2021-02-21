#lang sicp
;; map-tree layer
(define (map-tree op tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (op tree))
        (else
         (cons (map-tree op (car tree)) (map-tree op (cdr tree))))))

(define (scale-tree tree factor)(map-tree (lambda(x)(* x factor)) tree))
(scale-tree (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7)) 10)

(define (square-tree tree) (map-tree (lambda(x)(* x x)) tree))
(square-tree (list 1
                  (list 2 (list 3 4) 5)
                  (list 6 7)) 10)
