#lang sicp
(#%require sicp-pict)
(define e einstein)
(define (flipped-pairs painter)
  (let ((painter2
         (beside painter
                (flip-vert painter))))
    (below painter2 painter2)))
(paint (flipped-pairs e))
;(define (right-split painter)
;  (let ((right
;         (below painter
;                 (flip-vert painter))))
;    (beside painter right)))
(define (right-split painter n)
  (if (= n 0)
     painter
     (let ((smaller (right-split painter
                                (- n 1))))
       (beside painter
              (below smaller smaller)))))
(define (left-split painter n)
  (if (= n 0)
     painter
     (let ((smaller (left-split painter
                               (- n 1))))
       (beside (below smaller smaller)
              painter))))
(define (up-split painter n)
  (if (= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
       (below painter (beside smaller smaller)))))
(define (down-split painter n)
  (if (= n 0)
     painter
     (let ((smaller (up-split painter (- n 1))))
       (below smaller painter))))

;(paint (up-split e 5))
(define (corner-split painter n)
  (if (= n 0)
     painter
     (let ((up (up-split painter (- n 1)))
           (right (right-split painter
                              (- n 1))))
       (let ((top-left (beside up up))
             (bottom-right (below right right))
             (smaller (corner-split painter (- n 1))))
         (beside  (below painter top-left)
                 (below bottom-right
                       smaller)
                 )))))
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter)
                       quarter)))
      (below (flip-vert half) half))))
(paint (square-limit e 2))
(paint (corner-split e 10))

;;;;;;; higher order
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter)
                      (tr painter)))
          (bottom (beside (bl painter)
                         (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4
         (square-of-four identity
                        flip-vert
                        rotate180
                        flip-vert)))
    (combine4 painter)))

(define (square-limit2 painter n)
  (let ((combine4
         (square-of-four flip-horiz
                        identity
                        rotate180
                        flip-vert)))
    (combine4 (corner-split painter n))))
    ;(paint (right-split e 1))
    ;(paint (flipped-pairs e))

(define (split op_first op_second) (lambda(pict n)
  (if (= n 0)
     pict
     (let ((split-elem ((split op_first op_second) pict (- n 1))))
       (op_first pict
                   (op_second split-elem split-elem))))))
(define right-split-high (split beside below))
(define up-split-high (split below beside))
