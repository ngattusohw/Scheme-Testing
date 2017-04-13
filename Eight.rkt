#lang eopl

;;helper methods provided by exercise
(define cycle6 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 1)))
(define test '((1 2) (2 3) (2 1) (3 3) (4 5) (5 2)))
(define EQrel10 '((1 1) (1 2) (2 2) (2 1) (3 3) (4 4) (4 5) (4 6) (5 4) (6 4) (5 5) (6 6) (5 6) (6 5) (7 7) (7 8) (8 7) (7 9) (9 7) (7 10) (10 7) (8 8) (8 9) (9 8) (8 10) (10 8) (9 9) (9 10) (10 9) (10 10)))

(define (element? item list-of-items)
  (if (null? list-of-items)                  ;Is our "set" empty?
      #f                                     ;If empty, not an element!
      (if (equal? item (car list-of-items))  ;Is our item first in list?
          #t                                 ;Yes?  Then it's an element!
          (element? item (cdr list-of-items)))));No? Check the rest.

(define (make-set list-of-items)
  (if (null? list-of-items) ;An empty list can have no duplicates,
      '()                   ;so just return an empty list.
      (if (element? (car list-of-items) (cdr list-of-items))
          (make-set (cdr list-of-items))
          (cons (car list-of-items) (make-set (cdr list-of-items))))))

(define (compose relationOuter relationInner)
  (make-set (Compose relationOuter relationOuter relationInner)))

(define (Compose pristineOuter relationOuter relationInner)
  (if (null? relationInner)
      '()
      (if (null? relationOuter)
          (Compose pristineOuter pristineOuter (cdr relationInner))
          (if (equal? (cadar relationInner) (caar relationOuter))
              (cons (list (caar relationInner) (cadar relationOuter))
                    (Compose pristineOuter 
                             (cdr relationOuter) 
                             relationInner))
              (Compose pristineOuter 
                             (cdr relationOuter) 
                             relationInner)))))

(define (relation? list-of-duples-we-hope)
  (if (null? list-of-duples-we-hope)
      #t
      (if (or (not (list? (car list-of-duples-we-hope)))
              (not (= 2 (length (car list-of-duples-we-hope)))))
          #f
          (relation? (cdr list-of-duples-we-hope)))))

(define (id n)
  (if (= 0 n)
     '()
     (cons (list n n) (id (- n 1)))))


(define (reflexive? relation n)  
  (subset? (id n) relation))

(define (R-minus-1 relation)
  (if (null? relation)
      '()
      (cons (reverse (car relation)) (R-minus-1 (cdr relation)))))

(define (symmetric? relation)
  (subset? (R-minus-1 relation) relation))

(define (symmetric-closure relation) 
  (union relation (R-minus-1 relation)))

(define (related-to element relation)
  (if (null? relation)
      '()
      (if (equal? element (caar relation))
          (cons (cadar relation) (related-to element (cdr relation)))
          (related-to element (cdr relation)))))

(define (union setA setB)
  (make-set (append setA setB))) 

(define (intersection setA setB)
  (make-set (Intersection (make-set setA) (make-set setB))))

(define (Intersection setA setB)
  (if (null? setA) 
      '()
      (if (element? (car setA) setB)
          (cons (car setA) (intersection (cdr setA) setB))
          (intersection (cdr setA) setB))))

(define (subset? setA setB)
  (if (null? setA)
      #t
      (if (element? (car setA) setB)
          (subset? (cdr setA)  setB)
          #f)))

(define (set-equal? setA setB)
   (and (subset? setA setB) (subset? setB setA)))

(define (proper-subset? setA setB)
  (and (subset? setA setB) (not (set-equal? setA setB))))

(define (set-difference setA setB)
  (make-set (Set-Difference setA setB)))

(define (Set-Difference setA setB)
  (if (null? setA)
      '()
      (if (element? (car setA) setB)
          (Set-Difference (cdr setA) setB)
          (cons (car setA) (Set-Difference (cdr setA) setB)))))

(define (sym-diff setA setB)
  (union (set-difference setA setB) (set-difference setB setA)))

(define (cardinality set)
  (length (make-set set)))

(define (disjoint? setA setB)
  (null? (intersection setA setB)))

(define (superset? setA setB)
  (subset? setB setA))

(define (insert element set)
  (make-set (cons element set)))

(define (remove element set)
  (set-difference set (list element)))

(define (transitive? relation n)
  (subset? (transitive-closure relation n) relation))

;;end helper

;;test with (transitive-closure test 5)
(define (transitive-closure relation n)
  (if (equal? n 0)
      '()
      (union(power relation n)(transitive-closure relation(- n 1)))))



(define (Power original power power-so-far k)
  (if(equal? power-so-far k)
     power
     (Power original (compose original power) (+ power-so-far 1) k)))
;;test with (power cycle6 1)
(define (power relation k)
  (Power relation relation 1 k))

;;test with (EQ-relation? EQrel10 10)
(define (EQ-relation? relation n)
  (and (relation? relation) (reflexive? relation n) (transitive? relation n) (symmetric? relation)))
