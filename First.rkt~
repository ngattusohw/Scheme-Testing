#lang racket


(define (3majority p q r)
  (and (or p r) (and (or p q) (or q r))))

(define (isosceles p q r)
  (and (or (not p) (not q) (not r)) (and (or p q) (or q r))))