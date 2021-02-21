#lang sicp
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))


(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq) (accumulate op
                              initial
                              (cdr seq)))))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


;generate a sequence of [i, j]
(define (enumerate-interval i j)
  (define (iter k seq)
    (if (> k j)
      seq
      (iter (+ k 1) (append seq (list k)))))
  (iter i nil))


(define (queens board-size)
  ; represent an empty-board
  (define empty-board '())


  (define (safe? k positions)
    (define (safe-it i positions row-of-new-queen)
      ; 'i is the step
      (if (null? positions)
        #t
        (let ((row-of-current-queen (car positions)))
          (if (or (= row-of-new-queen row-of-current-queen)
                  (= (+ row-of-current-queen i) row-of-new-queen)  ; downward collision
                  (= (- row-of-current-queen i) row-of-new-queen)) ; upward collision
            #f
            (safe-it (+ i 1) (cdr positions) row-of-new-queen)))))


    (safe-it 1 (cdr positions) (car positions)))


  ; add a queen to board
  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))


  ; put queens in the first k columns
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      ; choose positions which are safe
      (filter
        (lambda (positions)
          (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position
                                     new-row
                                     k
                                     rest-of-queens))
                 ; generate row sequence
                 (enumerate-interval
                   1
                   board-size)))
          (queen-cols (- k 1))))))


  (queen-cols board-size))

(for-each (lambda (positions)
            (begin
              (display positions)
              (newline))) (queens 6))
