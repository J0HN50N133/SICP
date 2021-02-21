#lang sicp
(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op
                              initial
                              (cdr seq)))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (permutations s)
  ; remove elem 'x' from sequence 'seq'
  (define (remove x seq)
    (flatmap
      (lambda(i) (if (= i x) '() (list i)))
      seq))

  (if (null? s)
    (list nil)
    (flatmap (lambda(x)
               (map (lambda(p)
                      (cons x p))
                    (permutations
                      (remove x s))))
             s)))

(permutations (list 1 2 3 4 5))

