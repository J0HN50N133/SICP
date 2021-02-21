#lang sicp

(#%require sicp-pict)
;;;;------------ vector ------------

(define (make-vect xcor ycor) (cons xcor ycor))
(define (xcor-vect vect)(car vect))
(define (ycor-vect vect)(cdr vect))
(define (add-vect v1 v2)(cons (+ (xcor-vect v1) (xcor-vect v2))
                             (+ (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect scale vect)(cons (* scale (xcor-vect vect))
                                    (* scale (ycor-vect vect))))
(define (minus-vect v1 v2)(add-vect v1 (scale-vect -1 v2)))
;;;;;-------------------------------

;;;;----------- frame --------------
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)(car frame))
(define (edge1-frame frame)(cadr frame))
(define (edge2-frame frame)(caddr frame))

; frame-coord-map : frame -> (vect -> vect)
;    frame-coord-map: gives a vector returns a vector

(define (frame-coord-map frame)
  (lambda(v)
    (add-vect (origin-frame frame)
             (add-vect
              (scale-vect (xcor-vect v)
                         (edge1-frame frame))
              (scale-vect (ycor-vect v)
                         (edge2-frame frame))))))
;;;;;-------------------------------


;;;;----------- segment ------------

(define (make-segment start-vect end-vect)
  (cons start-vect end-vect))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))
; (define (draw-line s)())

;;;;--------------------------------

;;;;---------- painter -------------
;(define (segments->painter segment-list)
;  (lambda(frame)
;    (let ((canvas (frame-coord-map frame)))
;      (for-each
;       (lambda (segment)
;         (draw-line
;          (canvas (start-segment segment))
;          (canvas (end-segment segment))))
;       segment-list))))


(segments->painter)