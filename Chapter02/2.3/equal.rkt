#lang sicp

(define (equal? x y)
  (cond ((and (symbol? x)
              (symbol? y))
         (and (eq? x y)))
        ((and (list? x)
              (list? y))
         (list-equal x y))
        (else
          (error "Wrong type input x and y -- EQUAL?" x y))))


(define (list-equal seq1 seq2)
  (cond 
    ; both sequences are null
    ((and (null? seq1)
          (null? seq2))
     #t)
    ; one of sequences is null
    ((or (null? seq1)
         (null? seq2))
     #f)
    (else (and (equal? (car seq1)
                       (car seq2))
               (equal? (cdr seq1)
                       (cdr seq2))))))
;(symbol? 'a)
;(equal? '(this is a list)
        ;'(this is a list))
;(equal? (list '1 '2 '3) (list '1 '2 '3))
