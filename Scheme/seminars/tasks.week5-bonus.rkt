#lang racket

(define (digit-counter n)
  (if (= n 0)
      0
      (+ 1 (digit-counter (quotient n 10)))))

;;; Triffon's tasks 

;;; Task1
(define (middle-digit n)
  (define digits (digit-counter n))
  (if (even? digits)
      #f
      (remainder (quotient n (expt 10 (quotient digits 2))) 10)))



(define (all? p? l)
  (if (null? l)
      #t
      (if (p? (car l) )
          (all? p? (cdr l))
          #f)))

;;; Task2
(define (is-em? l op f)
  (define help1
    (all? (lambda (x)
            (member (f x) l))))
  (define help2
    (all? (lambda (x)
          (all? (lambda (y)
                (= (op (f x) (f y)) (f (op x y)))) l)) l))
  (and help1 help2))


(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ 1 a) b))))


(define (any? p? l)
  (if (null? l)
      #f
      (if (p? (car l))
          #t
          (any? p? (cdr l)))))


;;; Task3
(define (meet-twice? f g a b)
  (define l (from-to a b))
  (any? (lambda (x)
          (any? (lambda (y)
                  (and  (not (= x y))
                        (= (f x) (g x))
                        (= (f y) (g y)))) l)) l))


;;; Task4
(define (next-look-and-say l)
  (define (count x l1)
    (if (null? l1)
        0
        (if (= (car l1) x)
            (+ 1 (count x (cdr l1)) )
            0)))
  (if (null? l)
      l
     (let ((c (count (car l) l)))
      (cons c
            (cons (car l)
                  (next-look-and-say (drop l c)))))))

(define (foldl op nv l)
  (if (null? l)
      nv
      (foldl op (op nv (car l)) (cdr l))))

(define (atom? x) (and (not (null? x)) (not (list? x))))

(define (deep-foldl op nv term l)
  (foldl op nv (map (lambda (x)
                      (if (atom? x)
                          (term x)
                          (deep-foldl op nv term x))) l)))



;;; Deyan's tasks

;;; Task1
(define (calcPoly l x)
  (foldl (lambda (u v) (+ (* u x) v) )
         0
         l))

(define l1 '(1 2 3 4 5))

(define l2 '(4 5 6 7 8))

(define (member? x l)
  (and (not (null? l))
      (if (= x (car l))
          #t
          (member? x (cdr l)))))

;;; Task2
(define (interception l1 l2)
   (if (null? l1)
       '()
       (if (member? (car l1) l2)
           (cons (car l1) (interception (cdr l1) l2))
           (interception (cdr l1) l2))))


(define (union l1 l2)
  (if (null? l1)
      l2
      (if (member? (car l1) l2)
          (union (cdr l1) l2)
          (cons (car l1) (union (cdr l1) l2)))))


(define (set-minus l1 l2)
  (if (null? l1)
      l1
      (if (member? (car l1) l2)
          (set-minus (cdr l1) l2)
          (cons (car l1) (set-minus (cdr l1) l2)))))



(define (getLastDigit n)
  (remainder n 10))

(define (getFirstDigit n)
  (quotient n (expt 10 (- (digit-counter n) 1))))

;;; Task3
(define (numGame? l)
  (define (help prevLastDigit currL)
    (if (null? currL)
        #t
        (if (= (getFirstDigit (car currL)) prevLastDigit)
            (help (getLastDigit (car currL)) (cdr currL))
            #f)))
  (and (not (null? l))
      (help (getLastDigit (car l)) (cdr l))))



(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define 1+ (lambda (x) (+ 1 x)))
(define (sq x) (* x x))
(define id (lambda (x) x))

;;; Task4
(define (generate a b l)
   (accumulate (lambda (x y) (if (member? (sq x) l)
                                 (cons x y)
                                 y))
               '()
               a
               b
               id
               1+))



(define (lastElement l)
  (if (null? l)
      '()
      (if (null? (cdr l))
          (car l)
          (lastElement (cdr l)))))
(define (push-back x l)
  (append l (list x)))

(define (getLarger interval1 interval2)
  (if (> (length interval1) (length interval2))
      interval1
      interval2))

(define (print result)
  (if (null? result)
      '()
      (cons (car result) (lastElement result))))

;;; Task5
(define (largestInterval f g a b)
  (define (help max curr currA)
    (if (> currA b)
        (print (getLarger max curr)) 
        (if (= (f currA) (g currA))
            (help max (push-back currA curr) (+ 1 currA))
            (if (> (length curr) (length max))
                (help curr '() (+ 1 currA))
                (help max '() (+ 1 currA))))))
  (help '() '() a))


;;; Fibonacci iterative
(define (fib n)
  (define (help i fib1 fib2)
    (if (< i n)
        (help (+ 1 i) fib2 (+ fib1 fib2))
        fib2))
  (if (= n 0)
      0
      (help 1 0 1)))


;;; N choose K using only accumulate
(define (nchk n k)
  (accumulate *
              1
              1
              k
              (lambda (i) (/ (- (+ n 1) i) i))
              (lambda (x) (+ 1 x))))


(define (2^ n)
  (accumulate *
              1
              1
              n
              (lambda (i) 2)
              1+))

(define (dec-to-bin n)
  (define (help mult currN res)
    (if (= currN 0)
        res
        (help (+ 1 mult)
              (quotient currN 2)
              (+ res (* (remainder currN 2) (expt 10 mult))))))
  (help 0 n 0))



(define (deep-foldl* op nv term l)
  (foldl op nv
         (map (lambda (x) (if (atom? x)
                              (term x)
                              (deep-foldl* op nv term x))) l)))


