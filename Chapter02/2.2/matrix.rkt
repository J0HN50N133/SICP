#lang sicp
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op
                                initial
                                (cdr seq)))))
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
; -------------- accumulate function ---------------
; -------------- accumulate function test ---------------
;(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (for-each op sequence)
  (if (null? (cdr sequence))
      nil
      (and (op (car sequence)) (for-each op sequence))))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define m2 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define v(list 1 2 3 4))

; -------------- dot-product function ---------------
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; -------------- dot-product test ---------------
;(dot-product (list 1 2 3)(list 1 1 1))

; -------------- transpose function ---------------
(define (transpose mat)
  (accumulate-n cons nil mat))
; -------------- transpose test ---------------
;(transpose m)

; -------------- matrix-*-vector function ---------------
(define (matrix-*-vector m v)
  (map (lambda(x)(dot-product x v)) m))
; -------------- matrix-*-vector function ---------------
;(matrix-*-vector m v)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(x)(matrix-*-vector cols x)) m)))
;(matrix-*-matrix m m2)
