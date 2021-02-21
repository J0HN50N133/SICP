#lang sicp

;;;
;;; The symbolic differentiation program.
;;;

;;;
;;; Representing algebraic expression
;;;


(define (variable? x) (symbol? x))

; same-variable? : Two variables are the same 
; if the symbols representing them are eq?
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; basic function
;;; universe selector
(define (operator e) (car e))
(define (first-term e) (cadr e))
(define (second-term e) (caddr e))
(define (tail-operand e) (cddr e))
;;; Is the exp equal to num
(define (=number? exp num)
  (and (number? exp) (= exp num)))
; does the expression have only one operand?
(define (single-operand? exp) (null? (cdr exp)))

;; representing of sum
(define (make-sum a1 . tail)
  (if (single-operand? tail)
    (let ((a2 (car tail)))
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2))
             (+ a1 a2))
            (else (list '+ a1 a2))))
    (cons '+ (cons a1 tail))))

(define (sum? exp)
  (and (list? exp)
       (eq? (operator exp) '+)))
(define (augend sum-exp) (first-term sum-exp))
(define (addend sum-exp)
  (let ((tail (tail-operand sum-exp)))
    (if (single-operand? tail)
      (car tail)
      (apply make-sum tail))))

;; representing of minus
(define (make-minus exp1 exp2)
  (cond ((=number? exp1 0) (make-product -1 exp2))
        ((=number? exp2 0) exp1)
        ((and (number? exp1) (number? exp2))
         (- exp1 exp2))
        (else (list '- exp1 exp2))))

(define (minus? exp)
  (and (list? exp)
       (eq? (operator exp) '-)))
(define (minuend sum-exp) (first-term sum-exp))
(define (subtrahend sum-exp) (second-term sum-exp))

;; representing of product
(define (make-product m1 . tail)
  (if (single-operand? tail)
    (let ((m2 (car tail)))
      (cond ((or (=number? m1 0)
                 (=number? m2 0))
             0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2))
             (* m1 m2))
            (else (list '* m1 m2))))
    (cons '* (cons m1 tail))))
(define (product? exp)
  (and (list? exp)
       (eq? (operator exp) '*)))
(define (multiplicand exp) (first-term exp))
(define (multiplier exp)
  (let ((tail (tail-operand exp)))
    (if (single-operand? tail)
      (car tail)
      (apply make-product tail))))

; representing of sin
(define (make-sin exp)
  (if (number? exp)
    (sin exp)
    (list 'sin exp)))
  
(define (sin? exp)
  (and (list? exp)
       (eq? (operator exp) 'sin)))
(define (sin-content exp) (first-term exp))

; representing of cos
(define (make-cos exp)
  (if (number? exp)
    (cos exp)
    (list 'cos exp)))
(define (cos? exp)
  (and (list? exp)
       (eq? (operator exp) 'cos)))
(define (cos-content exp) (first-term exp))

; representing of exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((number? base) (expt base exponent))
    (else(list '** base exponent))))
(define (exponentiation? exp)
  (eq? (operator exp) '**))
(define (base exp) (first-term exp))
(define (exponent exp) (second-term exp))

;;;
;;; main program
;;;


; TODO: support arbitray arguments numbers of terms
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (augend exp) var)
                   (deriv (addend exp) var)))
        ((minus? exp)
         (make-minus (deriv (minuend exp) var)
                     (deriv (subtrahend exp) var)))
        ((product? exp)
         (make-sum
           (make-product
             (multiplicand exp)
             (deriv (multiplier exp) var))
           (make-product
             (multiplier exp)
             (deriv (multiplicand exp) var))))
        ((cos? exp)
         (make-product -1
                       (make-product (deriv (cos-content exp) var)
                                     (make-sin (cos-content exp)))))
        ((sin? exp)
         (make-product (deriv (sin-content exp) var)
                       (make-cos (sin-content exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else (error "unknown expression type -- DERIV" exp))))

;(deriv '(* y (** x 2)) 'x)
(deriv '(* x y (+ x 3)) 'x)
;(deriv '(* x y) 'x)
;(make-product 'x 'y '(** x 2))
;(make-sum 'x 'y '(+ x 3))
;(define (f a . b) (single-operand? b))
;(f 'x 0)
