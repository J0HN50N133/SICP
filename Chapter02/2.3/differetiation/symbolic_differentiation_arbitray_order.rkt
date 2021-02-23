#lang sicp

;;;
;;; The symbolic differentiation program.
;;;

;;;
;;; Representing algebraic expression
;;;

(define exp-order 'infix);prefix infix suffix
(define (variable? x) (symbol? x))

; same-variable? : Two variables are the same 
; if the symbols representing them are eq?
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; basic function
;;; universe selector
(define (binary-operator exp)
  (cond ((eq? exp-order 'prefix) (car exp))
        ((eq? exp-order 'infix) (cadr exp))
        ((eq? exp-order 'suffix) (caddr exp))))
(define (unary-operator exp)
  (if (eq? exp-order 'suffix)
    (cadr exp)
    (car exp)))
(define (first-term-of-binary exp)
  (cond ((eq? exp-order 'prefix) (cadr exp))
        ((eq? exp-order 'infix) (car exp))
        ((eq? exp-order 'suffix) (car exp))))
(define (second-term-of-binary exp)
  (cond ((eq? exp-order 'prefix) (caddr exp))
        ((eq? exp-order 'infix) (caddr exp))
        ((eq? exp-order 'suffix) (cadr exp))))
(define (content-of-unary exp) 
  (if (eq? exp-order 'suffix)
    (car exp)
    (cadr exp)))

;;; universe constructor
(define (binary-constructor operator first-term second-term)
  (cond ((eq? exp-order 'prefix) (list operator first-term second-term))
        ((eq? exp-order 'infix) (list first-term operator second-term))
        ((eq? exp-order 'suffix) (list first-term second-term operator))))
(define (unary-constructor operator content)
  (if (eq? exp-order 'suffix)
    (list content operator)
    (list operator content)))

;;; Is the exp equal to num
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; representing of sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (binary-constructor '+ a1 a2))))
(define (sum? exp)
  (and (list? exp)
       (eq? (binary-operator exp) '+)))
(define (augend sum-exp) (first-term-of-binary sum-exp))
(define (addend sum-exp) (second-term-of-binary sum-exp))

;; representing of minus
(define (make-minus exp1 exp2)
  (cond ((=number? exp1 0) (make-product -1 exp2))
        ((=number? exp2 0) exp1)
        ((and (number? exp1) (number? exp2))
         (- exp1 exp2))
        (else (binary-constructor '- exp1 exp2))))

(define (minus? exp)
  (and (list? exp)
       (eq? (binary-operator exp) '-)))
(define (minuend sum-exp) (first-term-of-binary sum-exp))
(define (subtrahend sum-exp) (second-term-of-binary sum-exp))

;; representing of product
(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (binary-constructor '* m1 m2))))
(define (product? exp)
  (and (list? exp)
       (eq? (binary-operator exp) '*)))
(define (multiplicand exp) (first-term-of-binary exp))
(define (multiplier exp)(second-term-of-binary exp))

; representing of sin
(define (make-sin exp)
  (if (number? exp)
    (sin exp)
    (unary-constructor 'sin exp)))
  
(define (sin? exp)
  (and (list? exp)
       (eq? (unary-operator exp) 'sin)))
(define (sin-content exp) (content-of-unary exp))

; representing of cos
(define (make-cos exp)
  (if (number? exp)
    (cos exp)
    (unary-constructor 'cos exp)))
(define (cos? exp)
  (and (list? exp)
       (eq? (unary-operator exp) 'cos)))
(define (cos-content exp) (content-of-unary exp))

; representing of exponentiation
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((number? base) (expt base exponent))
    (else(binary-constructor '** base exponent))))
(define (exponentiation? exp)
  (eq? (binary-operator exp) '**))
(define (base exp) (first-term-of-binary exp))
(define (exponent exp) (second-term-of-binary exp))

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
;(deriv '(x + (x ** 3)) 'x)
;(deriv '(x + (x + x)) 'x)
;(deriv '(x (x 3 **) +) 'x)
