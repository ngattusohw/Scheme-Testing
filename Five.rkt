#lang racket

(define (pig-latin word)
   (if (null? word)
        '()
        (if (equal? (car word) 'q)
            (append (cddr word) (cons (car word) '(u a y)))
            (append (cdr word) (cons (car word) '(a y))))))

(define (Weight w)
  (* 0.453592 w))

(define (Height h)
  (* 0.0254 h))

(define (CalcBMI Weight Height)
  (/ Weight (* Height Height)))
         
(define (BMI h w)
  (define bmi (CalcBMI (Weight w) (Height h)))
  (display bmi)
  (cond ((> 16 bmi) (display "Severely Underweight"))
      ((<= 16 bmi 18.5) (display "Underweight"))
      ((<= 18.5 bmi 25) (display "Normal"))
      ((<= 25 bmi 30) (display "Overweight"))
      ((<= 30 bmi 40) (display "Obese"))
      (else
       (display "Morbidly Obese"))))

(define (arithmetic-prog a_0 difference iNdex)
  (if (equal? 0 iNdex)
      '()
      (cons a_0 (arithmetic-prog(+ a_0 difference) difference (- iNdex 1)))))

(define (geometric-prog a_0 ratio iNdex)
  (if (equal? 0 iNdex)
      '()
      (cons a_0 (geometric-prog(* a_0 ratio) ratio (- iNdex 1)))))

(define (sum list-of-nums)
  (if (null? list-of-nums)
      '0
      (+ (car list-of-nums) (sum(cdr list-of-nums)))))
