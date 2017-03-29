#lang racket

;;test with (keep-evens '(2 4 6))
(define (keep-evens list-of-ints)
  (if (null? list-of-ints)
      '()
      (if (even? (car list-of-ints))
          (cons (car list-of-ints) (keep-evens (cdr list-of-ints)))
          (keep-evens (cdr list-of-ints)))))

(define (keep-odds list-of-ints)
  (if (null? list-of-ints)
      '()
      (if (odd? (car list-of-ints))
          (cons (car list-of-ints) (keep-odds (cdr list-of-ints)))
          (keep-odds (cdr list-of-ints)))))

(define (keep-ints list-of-ints)
  (if (null? list-of-ints)
      '()
      (if (integer? (car list-of-ints))
          (cons (car list-of-ints) (keep-ints (cdr list-of-ints)))
          (keep-ints (cdr list-of-ints)))))

(define (keep-zero list-of-ints)
  (if (null? list-of-ints)
      '()
      (if (zero? (car list-of-ints))
          (cons (car list-of-ints) (keep-zero (cdr list-of-ints)))
          (keep-zero (cdr list-of-ints)))))

;;test with (filter integer? '(1 2.72 5 0 3.14 1.41 72))
(define (filter predicate? list-of-things)
  (if (null? list-of-things)
      '()
      (if (predicate? (car list-of-things))
          (cons (car list-of-things) (filter predicate? (cdr list-of-things)))
          (filter predicate? (cdr list-of-things)))))

;;test with (EuclidAlgo 1701 3768)
(define (EuclidAlgo a d)
  (if(= d 0)
     a
     (EuclidAlgo d (modulo a d))))

;;test with (Euclid-list 72 48)
(define (Euclid-list a d)
  (if(= d 0)
     a
     (cons (modulo a d) (EuclidAlgo d (modulo a d)))))

;;test with (prime? 7)
(define (prime? n)
  (Prime? 2 n))

(define (Prime? possible-divisor n)
  (if (equal? possible-divisor n)
      #t
      (if (zero? (modulo n possible-divisor))
          #f
          (Prime? (+ 1 possible-divisor) n))))

;;test with (factor 64)
(define (factor n)
  (Factor 2 n))

(define (Factor divisor n)
  (if (prime? n)
      (list n)
      (if (equal? divisor n)
          '()
          (if (zero? (modulo n divisor))
              (cons divisor (Factor divisor (/ n divisor)))
              (Factor (+ 1 divisor) n)))))



