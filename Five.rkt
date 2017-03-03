#lang racket

(define (pig-latin word)
   (if (null? word)
        '()
        (if (equal? (car word) 'q)
            (append (cddr word) (cons (car word) '(u a y)))
            (append (cdr word) (cons (car word) '(a y))))))
;;test with (pig-latin '(q u i z))

(define (Weight w)
  (* 0.453592 w))
;;this is a helper function

(define (Height h)
  (* 0.0254 h))
;;this is a helper function

(define (CalcBMI Weight Height)
  (/ Weight (* Height Height)))
;;this is a helper function

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
;;test with (BMI 74 145)

(define (arithmetic-prog a_0 difference iNdex)
  (if (equal? 0 iNdex)
      '()
      (cons a_0 (arithmetic-prog(+ a_0 difference) difference (- iNdex 1)))))
;;test with (arithmetic-prog 2 2 2)

(define (geometric-prog a_0 ratio iNdex)
  (if (equal? 0 iNdex)
      '()
      (cons a_0 (geometric-prog(* a_0 ratio) ratio (- iNdex 1)))))
;;test with (geometric-prog 2 2 2)

(define (sum list-of-nums)
  (if (null? list-of-nums)
      '0
      (+ (car list-of-nums) (sum(cdr list-of-nums)))))
;;test with (sum '(2 3 4))

(define (arith-sum a_0 difference iNdex)
  (sum (arithmetic-prog a_0 difference iNdex)))
;;test with (arith-sum 2 2 2)

(define (geo-sum a_0 ratio iNdex)
  (sum (geometric-prog a_0 ratio iNdex)))
;;test with (geo-sum 2 6 2)

(define (make-duples item list-of-items)
  (if (null? list-of-items)
      '()
      (cons (list item (car list-of-items)) (make-duples item (cdr list-of-items)))))
;;test with (make-duples 0 '(1 2 3 4)) , (make-duples 6 '(1 2 3 4 3 2 1))

(define (cart-prod setA setB)
  (if (null? setA)
      '()
      (cons (make-duples(car setA) setB) (cart-prod (cdr setA) setB))))
;;test with (cart-prod '(1 2) '(a b c))







