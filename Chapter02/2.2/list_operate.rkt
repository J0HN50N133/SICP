#lang sicp
  ;wheter sequence 's contains elem x
(define (contains? x s)
  (if (null? s)
    #f
    (or (eq? x (car s))
        (contains? x (cdr s)))))

;check wheter seq has duplicate element
(define (no-repeat? seq)
  (if (null? seq)
    #t
    (and (not (contains? (car seq) (cdr seq)))
         (no-repeat? (cdr seq)))))

; check wheter the new positions is safe
; for queens problem
(define (safe? k positions)
  (define (downward pl)
    (map + pl (enumerate-interval 1 k)))
  (define (upward pl)
    (map - pl (enumerate-interval 1 k)))
  (and (no-repeat? positions)
       (no-repeat? (downward positions))
       (no-repeat? (upward positions))))

