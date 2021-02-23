#lang sicp

;;;
;;;operation of unordered set.
;;;

; union-set : set set -> set
; computes the union of two sets
; O(N * M), N is the size of s1, M is the size of s2
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
        (else (union-set (cdr s1) (cons (car s1) s2)))))

; intersection-set : set set -> set
; computes the intersection of two sets
; O(N * M), N is the size of s1, M is the size of s2
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

; element-of-set? : obj set -> boolean
; determines whether a given element 
; is a member of a set.
; O(N), N is the size of the set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; adjoin-set : obj set -> set
; return a set that contains the element
; of the original set and also adjoined element.
; O(N), N is the size of the original set
(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(display (intersection-set '(1 2 3 4 5) '(4 5 6 7)))
