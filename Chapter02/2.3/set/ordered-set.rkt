#lang sicp

;;;
;;;operation of ordered set.
;;;

; smallest-of-set : set -> obj
(define (smallest-of-set set) (if (null? set) nil (car set)))
; biggest-of-set : set -> obj
(define (biggest-of-set set) (if (null? set) nil (car (reverse set))))

; union-set : set set -> set
; computes the union of two sets
; O(N), N is the total size of two sets
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((< (biggest-of-set s2)
            (smallest-of-set s1)) (append s2 s1))
        ((< (biggest-of-set s1)
            (smallest-of-set s2)) (append s1 s2))
        (else (let ((x (car s1))
                    (y (car s2)))
                (cond ((= x y) (cons x (union-set (cdr s1) (cdr s2))))
                      ((< x y) (cons x (union-set (cdr s1) s2)))
                      ((> x y) (cons y (union-set s1 (cdr s2)))))))))

; intersection-set : set set -> set
; computes the intersection of two sets
; O(N), N is the total size of two sets
(define (intersection-set s1 s2)
  (if (or (null? s1)
          (null? s2)
          ; the biggest elem in s2 is smaller than the smallest elem in s1
          (< (biggest-of-set s2)
             (smallest-of-set s1))
          ; the biggest elem in s1 is smaller than the smallest elem in s2
          (< (biggest-of-set s1)
             (smallest-of-set s2)))
    '()
    (let ((x (car s1))
          (y (car s2)))
      (cond ((= x y) (cons x (intersection-set (cdr s1) (cdr s2))))
            ((< x y) (intersection-set (cdr s1) s2))
            ((> x y) (intersection-set s1 (cdr s2)))))))

; element-of-set? : obj set -> boolean
; determines whether a given element 
; is a member of a set.
; O(N), N is the size of the set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

; adjoin-set : obj set -> set
; return a set that contains the element
; of the original set and also adjoined element.
; O(N), N is the size of the original set
(define (adjoin-set x set)
  (define (iter first-half second-half)
    (if (or (null? second-half)
            (< x (car second-half)))
      (append first-half (cons x second-half))
      (iter (append first-half (list (car second-half))) (cdr second-half))))
  (if (element-of-set? x set)
    set
    (iter '() set)))


;;;
;;; Test cases
;;;

; adjoin-set
(adjoin-set 0 '(1 2 3 4))
(adjoin-set 5 '(1 2 3 4))

; union-set
(union-set '(1 5 7) '(2 5 7 8))
(union-set '(1 2 3 4 5) '(5 6 7 8))

; intersection-set
(intersection-set '(1 2 3 4) '(5 6 7 8))
(intersection-set '(1 5 7) '(2 5 7 8))
