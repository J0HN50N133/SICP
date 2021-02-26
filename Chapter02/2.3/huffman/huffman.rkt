#lang sicp

;;;
;;; Basic constructor and selector
;;;

;; leaf
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (and (list object)
       (eq? (car object)
            'leaf)))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

;; general tree
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

;; generic funtion

; symbols : list -> list
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
; weight
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; a general tree will be a list of a left branch,
; a right branch, a set of symbols, and a weight.

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch
                (car bits)
                current-branch)))
        (if (leaf? next-branch)
          (cons
            (symbol-leaf next-branch)
            ; go back to root
            (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits)
                    next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ; using generic funtion to support leaf and tree
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
          (cons (car set)
                (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      ; the set is ordered, sorting down by weight
      (adjoin-set
        (make-leaf (car pair)   ;symbol
                   (cadr pair)) ;frequency
        (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(define sample-msg-encoded '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-msg-encoded sample-tree)

(define (elements-of-tree? symbol tree)
  (define (elem-of-list? s l)
    (cond ((null? l) #f)
          ((eq? s (car l)) #t)
          (else (elem-of-list? s (cdr l)))))
  (elem-of-list? symbol (symbols tree)))

(define (encode message tree)
  (define (encode-1 symbol tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((leaf? tree) '())
            ((elements-of-tree?
               symbol
               left)
             (cons 0
                   (encode-1
                     symbol
                     left)))
            ((elements-of-tree?
               symbol
               right)
             (cons 1
                   (encode-1
                     symbol
                     right))))))
  (if (null? message)
    '()
    (append
      (encode-1 (car message)
                tree)
      (encode (cdr message) tree))))

(define sample-msg '(A D A B B C A))
(decode (encode sample-msg sample-tree) sample-tree)

;; generate a huffman tree
(define (generate-huffman-tree pairs)
  (define (successive-merge leaves)
    (cond ((= 0 (length leaves))
           '())
          ((= 1 (length leaves))
           (car leaves))
          (else
            (let ((new-subtree (make-code-tree
                                 (car leaves)
                                 (cadr leaves)))
                  (remained-ordered-set (cddr leaves)))
              (successive-merge
                (adjoin-set
                  new-subtree
                  remained-ordered-set))))))

  (successive-merge
    (make-leaf-set pairs)))

(define rock-tree (generate-huffman-tree '((A 2)
                                           (NA 16)
                                           (BOOM 1)
                                           (SHA 3)
                                           (GET 2)
                                           (YIP 9)
                                           (JOB 2)
                                           (WAH 1))))
(define msg-1 '(GET A JOB))
(define msg-2 '(SHA NA NA NA NA NA NA NA NA))
(define msg-3 '(WAH YIP YIP YIP YIP))
(define msg-4 '(YIP YIP YIP YIP YIP)')
(define msg-5 '(SHA BOOM))

