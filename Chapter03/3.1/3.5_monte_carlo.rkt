#lang sicp

(define (monte-carlo trials experiment)
  (define (iter remaining pass)
    (if (= remaining 0)
      (/ (* pass 1.0) trials)
      (if (experiment)
        (iter (- remaining 1) (+ pass 1))
        (iter (- remaining 1) pass))))
  (iter trials 0))

(define (pi-test)
  (< (+ (expt (random 1.0) 2) (expt (random 1.0) 2)) 1.0))

(* 4 (monte-carlo 100000000 pi-test))
