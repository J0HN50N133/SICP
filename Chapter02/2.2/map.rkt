#lang sicp
;;(map (lambda(x)(* x 2)) (list 1 2 3 4))
(define (last-pair lst)
        (if (null? (cdr lst))
          (car lst)
          (last-pair (cdr lst))))

(define (reverse-list lst)
        (if (null? (cdr lst))
        lst
        (append (reverse-list (cdr lst)) (list (car lst)))))

(last-pair (list 23 72 149 34))
(reverse-list (list 1 2 3 4 5 6 7 8))
