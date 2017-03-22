#lang eopl

(define TAmessage1 '((6 21 22 5) (25 14 15) (22 5) (6 1 1) (18 14 5 11)))
(define TAmessage2 '((15 18 11 22) (17 13 6) (11 22) (22 15 11 17 17) (12 13 22 7)))

;Actual calculation of 
(define (find-inv a k)
  (if (= 1 (modulo (* a k) 27)) ;only way out of infinite loop is if (a,27)=1
      k
      (find-inv a (+ 1 k))))

(define (a-inv a) ;NOTE:  if a and 27 are not relatively prime, this will enter infinite loop
  (if (< a 2)
      a
      (find-inv a 2)))

;Performs affine decryption word by word
(define (inv-aff-trans a-inv b numbers)
  (if (null? numbers)
      '()
      (cons (alpha (modulo (* (modulo (- 
                                       (car numbers) b) 27) a-inv) 27))  (inv-aff-trans a-inv b (cdr numbers)))))


(define (inv-affine message a b)
  (if (null? message)
      '()
      (cons (inv-aff-trans (a-inv a) b (car message)) (inv-affine (cdr message) a b))))


(define (letterword n numbers)
  (if (null? numbers)
      '()
      (cons (alpha (modulo (+ (- 27 n) (car numbers)) 27)) (letterword n (cdr numbers)))))

;Makes substitution number by number for letters
(define (alpha k)
  (cond ((= 0 k) '-)
        ((= 1 k) 'A)
        ((= 2 k) 'B)
        ((= 3 k) 'C)
        ((= 4 k) 'D)
        ((= 5 k) 'E)
        ((= 6 k) 'F)
        ((= 7 k) 'G)
        ((= 8 k) 'H)
        ((= 9 k) 'I)
        ((= 10 k) 'J)
        ((= 11 k) 'K)
        ((= 12 k) 'L)
        ((= 13 k) 'M)
        ((= 14 k) 'N)
        ((= 15 k) 'O)
        ((= 16 k) 'P)
        ((= 17 k) 'Q)
        ((= 18 k) 'R)
        ((= 19 k) 'S)
        ((= 20 k) 'T)
        ((= 21 k) 'U)
        ((= 22 k) 'V)
        ((= 23 k) 'W)
        ((= 24 k) 'X)
        ((= 25 k) 'Y)
        ((= 26 k) 'Z)))

(define (decryptall message)
  (decryptAll message 0))

(define (decryptAll message key)
  newline
  (if (= 27 key)
      (newline)
      (begin
        (newline)
        (display (decrypt message key))
        (decryptAll message (+ key 1)))))


;;
(define (convertHelp number base)
  (if (zero? number)
      '()
      (cons (modulo number base) (convertHelp (quotient number base) base))))

(define (convert number base)
  (reverse (convertHelp number base)))



(define (base10 oldbase numberlist)
  (if (null? numberlist)
      0
      (+ (* (car numberlist) (expt oldbase (- (length numberlist) 1))) (base10 oldbase (cdr numberlist)))
   ))
;;test with (base10 16 '(10 15)) or (base10 3 '(1 2 1 2))

(define (decrypt message key)
  (if (null? message)
      '()
      (cons (letterword key (car message)) (decrypt (cdr message) key))))
