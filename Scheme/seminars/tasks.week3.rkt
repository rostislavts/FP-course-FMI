;;; Task 1
(define (sum-step a b next)
  (define (help-iter currA result)
    (if (> currA b)
        result
        (help-iter (next currA) (+ result currA) ) ) )
  (help-iter a 0) )

(define (sum-term a b term)
  (define (help-iter currA result)
    (if (> currA b)
        result
        (help-iter (+ 1 currA) (+ result (term currA) ) )))
  (help-iter a 0))

(define (sum a b term next)
  (define (help-iter currA result)
    (if (> currA b)
        result
        (help-iter (next currA) (+ result (term currA) ) ) ))
  (help-iter a 0))

; From week2 ==================

(define (fact n)
  (define (for i result)
    (if (<= i n)
        (for (+ 1 i) (* result i))
          result ) )
  (for 1 1))


(define (pow-it x n)
  (define (iter i result)
    (if (< i n)
        (iter (+ i 1) (* result x) )
        result) )
  (iter 0 1))
(define (fast-pow x n)
  (if (< n 0)
      (pow-it (/ 1 x) (- n) )
      (pow-it x n)) )

(define (count-digits n)
  (define (abs x)
    (if (< x 0)
        (- x)
        x) )
  (define (iter result currN)
    (if (> currN 0)
        (iter (+ 1 result) (quotient currN 10) )
        result) )
  (if (= n 0)
      1
      (iter 0 (abs n) ) ))


(define (palindrome? n)
  (define (getDigitAt i)
     (remainder (quotient n (expt 10 (- (count-digits n) i) ) ) 10 ) )
  (define (iter idx)
    (if (< idx (- (count-digits n) (- idx 1)) )
        (if (= (getDigitAt idx) (getDigitAt (- (count-digits n) (- idx 1) ) ) )
            (iter (+ idx 1) )
            #f)
        #t) )
  (iter 1) )



;================================

;;; Task 2

(define (my-exp m)
  (lambda (x) (sum 0 m (lambda (n) (/ (pow-it x n) (fact n) ) ) (lambda (x) (+ 1 x) ) ) ))

(define (my-sin m)
  (lambda (x) (sum 0 m
                   (lambda (n) (* (pow-it (- 1) n) (/ (pow-it x (+ (* 2 n) 1) )  (fact (+ (* 2 n) 1) ) ) ))
                   (lambda (x) (+ 1 x) ) ) ) )


(define (my-cos m)
  (lambda (x) (sum 0 m
                   (lambda (n) (* (pow-it (- 1) n) (/ (pow-it x (* 2 n) )  (fact (* 2 n) ) ) ))
                   (lambda (x) (+ 1 x) ) ) ) )


;;; Task 3

(define (my-exp x m)
  (define (help-iter i result)
    (if (> i m )
        result
        (help-iter (+ 1 i) (+ result (/ (pow-it x i) (fact i) ) ) )) )
  (help-iter 0 0) )


;;; Task 4

(define (product a b term next)
  (define (help-iter currA result)
    (if (> currA b)
        result
        (help-iter (next currA) (* result (term currA) ) ) ))
 (help-iter a 1))


;;; Task 5

(define (sprod p)
  (lambda (x) (accumulate * 1 1 p
              (lambda (currA) (accumulate + 0 0 currA 
                          (lambda (k) (/ (* (pow-it (- 1) k) (pow-it x (+ (* 2 k ) 1) ) ) (fact (+ (* 2 k) 1) ) ) )
                          (lambda (k) (+ k 1 ) ) ))
              (lambda (k) (+ 1 k) ) )))


;;; Task 6

(define (accumulate op nv a b term next)
  (define (help-iter currA result)
   (if (> currA b)
       result
       (help-iter (next currA) (op result (term currA) ) ) ))
 (help-iter a nv))

(define (accumulate-rec op nv a b term next)
  (define (help currA)
   (if (> currA b)
       nv
       (op (term currA) (help (next currA) ) )))
 (help a))

;;; Task 7

(define (fact-acc n)
  (accumulate * 1 1 n (lambda (i) i)  (lambda (x) (+ 1 x))))

(define (pow-acc x n)
  (accumulate * 1 1 n (lambda (i) x ) (lambda (x) (+ 1 x) )))


;; Task 8

(define (count-palindromes a b)
  (define (help i counter)
    (if (> i b)
        counter
        (if (palindrome? i)
            (help (+ 1 i) (+ 1 counter) )
            (help (+ 1 i) counter) ) ) )
  (help a 0) )

;;; Task 9

(define (prime? n)
  (= (accumulate + 0 2 (- n 1)
              (lambda (k) (if (= (remainder n k) 0)
                              1
                              0) )
              (lambda (x) (+ 1 x) ) )
     0 ) )

;;; Task 10

(define (exists? pred? a b)
  (> (accumulate + 0 a b
              (lambda (i) (if (pred? i)
                              1
                              0) )
              (lambda (x) (+ 1 x) )) 0) )

;;; Task 11

(define (forall? pred? a b)
  (= (accumulate + 0 a b
              (lambda (i) (if (pred? i)
                              0
                              1) )
              (lambda (i) (+ 1 i) )) 0 ) )

;;; Task 12

(define (count-pred pred? a b next)
  (accumulate + 0 a b
              (lambda (i) (if (pred? i)
                              1
                              0) )
              next) )


;;; Task 13

(define (comb n k)
  (/ (fact-acc n) (* (fact-acc (- n k) ) (fact-acc k) ) ) )


;;; Task 14

(define (var n k)
  (/ (fact-acc n) (fact-acc (- n k) ) ) )

;;; Task 15

(define (flip f)
  (lambda (a b) (f b a) ) )

;;; Task 16

(define (twice f x) (f (f x)))
(define (compose f g) (f (g x)))

(define (repeated f n)
  (lambda (x) (define (help-iter i result)
    (if (< i n)
        (help-iter (+ 1 i) (f result) )
        result ))
    (help-iter 0 x) ))

(define (repeated-2 f n)
  (lambda (x) (define (help-iter i result)
    (if (< i n)
        (if (>= (- n i) 2)
            (help-iter (+ 2 i) (twice f result) ) 
            (help-iter (+ 1 i) (f result) ))
        result ))
    (help-iter 0 x) ))


;;; Task 17

(define (derive f dx)
  (lambda (x)
    (/ (- (f (+ x dx) ) (f x) ) dx ) ))


;;; Task 18


(define (derive-n f n dx)
  (lambda (x)
    (define (help i result)
      (if (< i n)
          (help (+ 1 i) (derive result dx) )
          (result x) ))
    (help 0 f) ))
