#lang sicp
(define (cc amount coin-list)
  (define (no-more? coin-list) (null? coin-list))
  (define (except-first-denomination coin-list) (cdr coin-list))
  (define (first-denomination coin-list) (car coin-list))
  (cond ((= amount 0)
         1)
        ((or (< amount 0) (no-more? coin-list))
         0)
        (else
         (+ (cc amount (except-first-denomination coin-list))
            (cc (- amount (first-denomination coin-list)) coin-list)))))

(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)
