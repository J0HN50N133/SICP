#lang sicp

;;;
;;; retrieval
;;;

; unordered list
(define (lookup-unordered-list given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else
          (lookup-unordered-list given-key
                  (cdr set-of-records)))))
