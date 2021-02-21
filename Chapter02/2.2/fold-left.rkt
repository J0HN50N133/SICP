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

;(fold-left / 1 (list 1 2 3))
;(fold-right / 1 (list 1 2 3))
;(fold-left list nil (list 1 2 3))
;(fold-right list nil (list 1 2 3))
;(define (reverse-right sequence)
  ;(fold-right (lambda(x y)(append y (list x))) nil sequence))
(define (reverse-right sequence)
  (fold-right (lambda(x y)(cons y (list x))) nil sequence))
(define (reverse-left sequence)
  (fold-left (lambda(x y)(cons y x)) '() sequence))

(reverse-right '(1 2 3 4))
(reverse-left (list 1 2 3 4 5 6 7))
