#lang racket


;;; Task1
;; a)
(define (divisor? x n)
  (and (not (= x 0)) (= (remainder n x) 0) ))

(define (sum-common-divisors n m)
  (define min-el (min m n))
  (define (help i res)
    (if (> i min-el)
        res
        (if (and (divisor? i m) (divisor? i n))
            (help (+ 1 i) (+ i res))
            (help (+ 1 i) res))))
  (help 1 0))

;; b)
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

(define (max-sum-fixed-arg x l)
  (define (help currL res)
    (if (null? currL)
        res
        (if (> (sum-common-divisors (car currL) x) res)
            (help (cdr currL) (sum-common-divisors (car currL) x))
            (help (cdr currL) res))))
  (help l 0))

(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ 1 a) b))))

(define (greatest-sum a b)
  (accumulate (lambda (x y) (max (max-sum-fixed-arg x (from-to (+ 1 x) b)) y))
              0
              a
              b
              (lambda (x) x)
              (lambda (i) (+ 1 i))))


;;; Task2
(define (foldr op nv l)
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))))

(define (result-list m ll)
  (if (null? ll)
      '()
      (cons (m (car ll)) (result-list m (cdr ll)))))

(define (good-metric? m ll)
  (define results (result-list m ll))
  (if (null? results)
      #t
      (let ((first (car results)))
      (equal? results (filter (lambda (x) (equal? first x)) results)))))

(define (count-metrics ml ll)
  (foldr (lambda (x y) (if (good-metric? x ll)
                           (+ 1 y)
                           y))
         0
         ml))

(define (prod l) (apply * l))
(define (sum l) (apply + l))


;;; Task3
(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (level-flatten dl)
  (define (help currDl atom-depth)
    (cond ((null? currDl) '())
          ((atom? currDl) (list (+ currDl atom-depth)))
          (else           (append (help (car currDl) (+ 1 atom-depth)) (help (cdr currDl) atom-depth)))))
  (help dl 0))
