#lang sicp

;;;
;;; TABLE
;;;

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;
;;; TAG
;;;

(define (attach-tag type-tag . contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "bad tagged datum: type-tag" datum)))
(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "bad tagged datum: contents" datum)))

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
(define (operator e) (type-tag e))
(define (operands e) (contents e))
;;; Is the exp equal to num
(define (=number? exp num)
  (and (number? exp) (= exp num)))


;; representing of sum
(define (install-sum-package)
  ;; internal procedures
  (define (augend s) (car s))
  (define (addend s) (cadr s))
  (define (make-sum x y)
    (cond ((=number? x 0) y)
          ((=number? y 0) x)
          ((and (number? x) (number? y))
           (+ x y))
          (else (attach-tag '+ x y))))
  ;; interface to the rest of the system
  (put 'first-operand '+ addend)
  (put 'second-operand '+ augend)
  (put 'constructor '+ make-sum)
  (put 'deriv '+
       (lambda(exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))))
  'install-sum-package-done)
(define (make-sum x y)
  ((get 'constructor '+) x y))
(define (addend sum)
  ((get 'first-operand '+) (contents sum)))
(define (augend sum)
  ((get 'second-operand '+) (contents sum)))

;; representing of minus
(define (install-minus-package)
  ;; internal procedures
  (define (minuend m) (car m))
  (define (subtrahend m) (cadr m))
  (define (make-minus x y)
    (cond ((=number? x 0) (make-product -1 y))
          ((=number? y 0) x)
          ((and (number? x) (number? y))
           (- x y))
          (else (attach-tag '- x y))))
  (put 'first-operand '- minuend)
  (put 'second-operand '- subtrahend)
  (put 'constructor '- make-minus)
  (put 'deriv '-
       (lambda (exp var)
         (make-minus (deriv (minuend exp) var)
                     (deriv (subtrahend exp) var))))
  'install-minus-package-done)
(define (make-minus x y)
  ((get 'constructor '-) x y))
(define (minuend e)
  ((get 'first-operand '-) (contents e)))
(define (subtrahend e)
  ((get 'second-operand '-) (contents e)))

;; representing of product
(define (install-product-package)
  ;; internal procedures
  (define (multiplicand p) (car p))
  (define (multiplier p) (cadr p))
  (define (make-product x y)
    (cond ((or (=number? x 0)
               (=number? y 0))
           0)
          ((=number? x 1) y)
          ((=number? y 1) x)
          ((and (number? x) (number? y)))
          (else (attach-tag '* x y))))
  (put 'first-operand '* multiplicand)
  (put 'second-operand '* multiplier)
  (put 'constructor '* make-product)
  (put 'deriv '*
       (lambda (exp var)
         (make-sum (make-product
                     (multiplier exp)
                     (deriv (multiplicand exp) var))
                   (make-product
                     (multiplicand exp)
                     (deriv (multiplier exp) var)))))
  'install-product-package-done)
(define (make-product x y)
  ((get 'constructor '*) x y))
(define (multiplicand e)
  ((get 'first-operand '*) (contents e)))
(define (multiplier e)
  ((get 'second-operand '*) (contents e)))

;; representing of sin
(define (install-sin-package)
  (define (sin-content exp)(car exp))
  (define (make-sin exp)
    (if (number? exp)
      (sin exp)
      (attach-tag 'sin exp)))
  (put 'first-operand 'sin sin-content)
  (put 'constructor 'sin make-sin)
  (put 'deriv 'sin
       (lambda(exp var)
         (make-product (deriv (sin-content exp) var)
                       (make-cos (sin-content exp)))))
  'install-sin-package-done)
(define (sin-content exp)
  ((get 'first-operand 'sin) (contents exp)))
(define (make-sin exp)
  ((get 'constructor 'sin) exp))
  
;; representing of cos
(define (install-cos-package)
  (define (cos-content exp)(car exp))
  (define (make-cos exp)
    (if (number? exp)
      (cos exp)
      (attach-tag 'cos exp)))
  (put 'first-operand 'cos cos-content)
  (put 'constructor 'cos make-cos)
  (put 'deriv 'cos
       (lambda(exp var)
         (make-product 
           -1
           (make-product (deriv (sin-content exp) var)
                         (make-cos (sin-content exp))))))
  'install-cos-package-done)
(define (cos-content exp)
  ((get 'first-operand 'cos) (contents exp)))
(define (make-cos exp)
  ((get 'constructor 'cos) exp))

;; representing of exponentiation
(define (install-exponentiation-package)
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base)
                (number? exponent))
           (expt base exponent))
          (else (attach-tag '** base exponent))))
  (define (base exp) (car exp))
  (define (exponent exp) (cadr exp))
  (put 'first-operand '** base)
  (put 'second-operand '** exponent)
  (put 'constructor '** make-exponentiation)
  (put 'deriv '**
       (lambda(exp var)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                       (base exp)
                                       (- (exponent exp) 1)))
                       (deriv (base exp) var))))
  'install-exponentiation-package-done)

(define (base exp)
  (get 'first-operand '**) (contents exp))
(define (exponent exp)
  (get 'second-operand '**) (contents exp))
(define (make-exponentiation base exponent)
  ((get 'constructor '**) base exponent))

;; representing of division
(define (install-division-package)
  (define (numer d)(car d))
  (define (denom d)(cadr d))
  (define (make-division x y)
    (cond ((=number? y 0)
           (error "ZeroDivisionError: division by zero -- make-division" x y))
          ((=number? x 0) 0)
          ((=number? y 1) x)
          ((and (number? x)
                (number? y))
           (/ x y))
          (else (attach-tag '/ x y))))
  (put 'first-operand '/ numer)
  (put 'second-operand '/ denom)
  (put 'constructor '/ make-division)
  (put 'deriv '/
       (lambda(exp var)
         (make-division
           (make-minus
             (make-product
               (deriv (numer exp) var)
               (denom exp))
             (make-product
               (numer exp)
               (deriv (denom exp) var)))
           (make-exponentiation (denom exp) 2))))
  'install-division-package-done)
(define (make-division x y)
  ((get 'constructor '/) x y))
(define (numer e)
  ((get 'first-operand '/) (contents e)))
(define (denom e)
  ((get 'second-operand '/) (contents e)))

;;;
;;; main program
;;;

; TODO: support arbitray arguments numbers of terms
(install-sum-package)
(install-minus-package)
(install-product-package)
(install-division-package)
(install-sin-package)
(install-cos-package)
(install-exponentiation-package)
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))
;(deriv '(** x 4) 'x)
(deriv '(/ 1 x) 'x)
