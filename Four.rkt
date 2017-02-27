#lang racket

(define (intersection setA setB)
  (if (null? setA) 
      '()
      (if (element? (car setA) setB)
          (cons (car setA) (intersection (cdr setA) setB))
          (intersection (cdr setA) setB))))

(define (make-set list-of-items)
  (if (null? list-of-items);An empty list can have no duplicates,
         '()                ;So just return an empty list.
        (if (element? (car list-of-items) (cdr list-of-items))
            (make-set (cdr list-of-items));We keep only the LAST item
            (cons (car list-of-items) (make-set (cdr list-of-items))))))

(define (element? item list-of-items)
  (if (null? list-of-items)                  ;Is our "set" empty?
      #f                                     ;If empty, not an element!
      (if (equal? item (car list-of-items))  ;Is our item first in list?
          #t                                 ;Yes?  Then it's an element!
          (element? item (cdr list-of-items)))));No? Check the rest.

(define (subset? setA setB)
  (if (null? setA) 
      #t
      (if (element? (car setA) setB)
         (subset? (cdr setA)  setB)
         #f)))

(define (union setA setB)
  (set-union setA setB))

(define (set-equal? setA setB)
  (and (subset? setA setB)(subset? setB setA)))
    
(define (proper-subset? setA setB)
  (and (subset? setA setB)(not (set-equal? setA setB))))
  

(define (Set-Difference setA setB)
  (if (null? setA)
      '()
      (if (element? (car setA) setB)
          (Set-Difference (cdr setA) setB)
          (cons (car setA) (Set-Difference (cdr setA) setB)))))

(define (set-difference setA setB)
  (make-set (Set-Difference setA setB)))

(define (sym-diff setA setB)
  (make-set(append (Set-Difference setA setB)(Set-Difference setB setA))))

(define (cardinality set)
  (length(make-set set)))

(define (disjoint? setA setB)
  (if (equal? 0 (cardinality (intersection setA setB))) #t #f))

(define (superset? setA setB)
  (subset? setB setA))

(define (insert element set)
  (make-set (cons element set)))

(define (remove element set)
  (sym-diff set (make-set(list element))))

