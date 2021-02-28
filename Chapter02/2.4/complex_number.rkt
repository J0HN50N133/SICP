#lang sicp

;;;;
;;;; type tag
;;;;

(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

;;;;
;;;; complex number representation
;;;;

;;;
;;; rectangular
;;;

(define (real-part-rectangular z)(car z))
(define (imag-part-rectangular z)(cdr z))

(define (square x) (* x x))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 
    'rectangular
    (cons (* r (cos a)) (* r (sin a)))))

;;;
;;; polar
;;;

(define (magnitude-polar z)(car z))
(define (angle-polar z) (cdr z))
(define (real-part-polar z)
  (* (magnitude-polar z)
     (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z)
     (sin (angle-polar z))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))
(define (make-from-real-imag-polar x y)
  (attach-tag
    'polar
    (cons (sqrt (+ (square x)
                   (square y)))
          (atan y x))))

;;;
;;; generic selector
;;;

; TODO: improve the scalability
(define (generic-selector datum
                          procedure-for-rectagular
                          procedure-for-polar
                          error-msg)
  (cond ((rectangular? datum)
         (procedure-for-rectagular (contents datum)))
        ((polar? datum)
         (procedure-for-polar (contents datum)))
        (else (error error-msg datum))))

(define (real-part z)
  (generic-selector z
                    real-part-rectangular
                    real-part-polar
                    "Unknown type: REAL-PART"))
(define (imag-part z)
  (generic-selector z
                    imag-part-rectangular
                    imag-part-polar
                    "Unknown type: IMAG-PART"))
(define (magnitude z)
  (generic-selector z
                    magnitude-rectangular
                    magnitude-polar
                    "Unknown type: MAGNITUDE"))
(define (angle z)
  (generic-selector z
                    angle-rectangular
                    angle-polar
                    "Unknown type: ANGLE"))

;;;
;;; constructor
;;;
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;;;
;;;; complex number operation
;;;;

;;;
;;; predicate
;;;

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;;;
;;; arithmic operation of complex number
;;;

;(make-from-real-imag (real-part z)
                     ;(imag-part z))
;(make-from-mag-ang (magnitude z)
                   ;(angle z))

(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))))

