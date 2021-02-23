#lang sicp

;;;
;;; binary tree set
;;;

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (if (null? set)
    (make-tree x '() '())
    (let ((e (entry set))
          (l (left-branch set))
          (r (right-branch set)))
      (cond ((= x e) set)
            ((< x e)
             (make-tree e (adjoin-set x l) r))
            ((> x e)
             (make-tree e l (adjoin-set x r)))))))

(define (biggest-of-set set)
  (let ((e (entry set))
        (r (right-branch set)))
    (if (null? r)
      e
      (biggest-of-set r))))

(define (smallest-of-set set)
  (let ((e (entry set))
        (l (left-branch set)))
    (if (null? l)
      e
      (smallest-of-set l))))

; tree->list-1 : tree -> list
; equal to pre-order traversal
; return the ordered list represented by the tree
; O(N^2) : for a tree have n node, append will be called
; n times, and the complexity of append is O(N)
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append
      (tree->list-1
        (left-branch tree))
      (cons (entry tree)
            (tree->list-1
              (right-branch tree))))))

; tree->list-2 : tree -> list
; equal to pre-order traversal
; return the ordered list represented by the tree
; O(N) : for a tree have n node, cons will be called
; n times, and the complexity of cons is O(1)

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list
        (left-branch tree)
        (cons (entry tree)
              (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

; partial-tree : list  -> (tree . tree)
; partial-tree : cut the list into two part, each part is a tree
; n : the size of left tree
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size
            (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree
                elts left-size)))
        ; get the left tree
        (let ((left-tree
                (car left-result))
        ; remain elts
              (non-left-elts
                (cdr left-result))
              (right-size
                (- n (+ left-size 1))))
          (let ((this-entry
                  (car non-left-elts))
                (right-result
                  (partial-tree
                    (cdr non-left-elts)
                  right-size)))
            (let ((righ-tree
                    (car right-result))
                  (remaining-elts
                    (cdr right-result)))
              (cons (make-tree this-entry
                               left-tree
                               righ-tree)
                    remaining-elts))))))))
; list->tree : ordered-list -> balanced binary tree
(define (list->tree elements)
  (car (partial-tree
         elements (length elements))))

; union-tree
; how to implement:
; 1. convert tree to list
; 2. using the union-set for ordered-list
; 3. convert the union result to tree

; intersection-tree
; how to implement:
; 1. convert tree to list
; 2. using the intersection-set for ordered-list
; 3. convert the intersection result to tree

; retrieval
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key
            (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key
            (key (entry set-of-records)))
         (lookup
           given-key
           (right-branch set-of-records)))
        ((< given-key
            (key (entry set-of-records)))
         (lookup
           given-key
           (left-branch set-of-records)))))

;;;
;;; Test cases
;;;

(define t (make-tree 1 '() '()))
(define new-t (adjoin-set 10 (adjoin-set 9 (adjoin-set 8 (adjoin-set 7 (adjoin-set 6 (adjoin-set 5 (adjoin-set 4 (adjoin-set 3 (adjoin-set 2 (adjoin-set 1 t)))))))))))
(tree->list-1 new-t)
(tree->list-2 new-t)

(list->tree '(1 3 5 7 9 11))

