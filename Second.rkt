#lang racket

(define ay '(a y))

(define (last test-list)
  (car (reverse test-list)))

(define (YODA yoda-list)
  (cons (car (reverse yoda-list)) (reverse(cdr (reverse yoda-list)))))

(define (pig-latin word-list)
   (append (append (cdr word-list) (list (car word-list))) ay))

(define student 
  '((IDnumber DegreeSought) (LastName FirstName) (day month year) (class/year ((major) (minor)) GPA) ((number street apt) (city state zip)) (class1 class2 ... classN)))

(define (IDnum student-name) 
  (caar student-name))

(define (lastname student-name)
  (car(cadr student-name)))

(define (GPA student-name)
  (cddr (car(cdddr student))))

(define (birthdate student-name)
  (car (cddr student-name)))
  
(define (address student-name)
  (car(cdr (reverse student-name))))

(define (class student-name)
  (cdr (car(cdddr student))))

(define (schedule student-name)
  (car(reverse student-name)))
  