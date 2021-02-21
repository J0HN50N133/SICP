#lang sicp

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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (prime-sum? pair)
  ; check whether 'num' is a prime number with fermat-test method
  (define (prime? num)
    (define (square x) (* x x))
    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (remainder (square (expmod base (/ exp 2) m))
                        m))
            (else
              (remainder (* base (expmod base (- exp 1) m)) m))))

  ; do fermat-test with n
    (define (fermat-test n)
      ; a is a number less than n
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))

  ; check whether n is prime by doing fermat-test n times
    (define (fast-prime? n times)
      (cond ((= times 0) true)
            ((fermat-test n) (fast-prime? n (- times 1)))
            (else false)))

    (fast-prime? num 100))

  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       ; (a,b) -> (a, b, a + b)
       (filter prime-sum?
               ; for each i, generate a sequence of number from 1 to i-1,
               ; then combine i with each number of the sequence.
               (flatmap (lambda(i)
                          (map 
                            (lambda(j) (list i j))
                            (enumerate-interval 1 (- i 1))))
               ; do the generate process for each number from 1 to n
                        (enumerate-interval 1 n)))))


;(prime-sum-pairs 100)
