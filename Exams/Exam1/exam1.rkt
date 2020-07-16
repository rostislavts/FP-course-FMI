#lang racket

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))


(define (accumulate-i op nv a b term next)
  (if (> a b)
      nv
      (accumulate op (op nv (term a)) (next a) b term next)))


;;; Task1

;; a)

;;; PROBLEM
(define (divisor? x n)
  (if (= x 0)
      #f
      (= (remainder n x) 0)))


;(define (sum-digit-divisors n)
 ; (accumulate (lambda (x y) (if (divisor? x n)
  ;                              (+ x y)
   ;                             y))
    ;          0
     ;         1
      ;        (abs n)
       ;       (lambda (x) x)
        ;      (lambda (x) (+ 1 x))))


;(define (sum-digit-divisors n)
 ; (define (help i res)
  ;  (if (> i n)
   ;     res
    ;    (if (divisor? i n)
     ;       (help (+ 1 i) (+ 1 res)) 
      ;      (help (+ 1 i) res ))))
  ;(help 1 0))


(define (digit-divisors d)
  (define (help i res)
    (if (> i 9)
        res
        (if (divisor? i d)
            (help (+ 1 i) (+ 1 res))
            (help (+ 1 i) res)) ))
  (help 1 0))

;;
;(define (sum-digit-divisors n)
 ; (if (= n 0)
  ;    0
   ;   (+ (digit-divisors (remainder n 10)) (sum-digit-divisors (quotient n 10)) ) ))
;;

;;

;; ======================


(define (sum-digit-divisors n)
  (define (help newN res)
    (if (= newN 0)
        res
        (if (divisor? (remainder newN 10) n )
            (help (quotient newN 10) (+ res (remainder newN 10)) )
            (help (quotient newN 10) res ))))
  (help n 0))


;; ======================

;; b)
(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ 1 a) b))))

(define (find-sum-fixed-first-arg x l)
  (define m (sum-digit-divisors x))
  (if (null? l)
      0
      (if (equal? m (sum-digit-divisors (car l)))
          (+ 1 (find-sum-fixed-first-arg x (cdr l)))
          (find-sum-fixed-first-arg x (cdr l)))))


;(define (same-sum a b)
 ; (accumulate (lambda (x y)
  ;              (+ (find-sum-fixed-first-arg x (from-to (+ 1 x) b) ) y))
   ;           0
    ;          a
     ;         b
      ;        (lambda (x) x)
       ;       (lambda (x) (+ 1 x))))



(define (same-sum a b)
  (define (help currA result)
    (if (> currA b)
        result
        (help (+ 1 currA) (+ result (find-sum-fixed-first-arg currA (from-to (+ 1 currA) b) ) ) ) ))
  (help a 0))



;;; Task2

(define (any? p? l)
  (if (null? l)
      #f
      (if (p? (car l))
          #t
          (any? p? (cdr l)))))


(define (all? p? l)
  (if (null? l)
      #t
      (if (p? (car l))
          (all? p? (cdr l))
          #f)))

;; ???
(define (best-metric? ml ll)
  (any? (lambda (m1)
          (all? (lambda (m2)
                  (all? (lambda (l)
                          (>= (m1 l) (m2 l)) )
                        ll))
                ml))
        ml))


(define (prod l) (apply * l))
(define (sum l) (apply + l))


;;; Task3

(define (atom? x)
  (and (not (null? x)) (not (pair? x)) ))

(define (deep-foldr op nv term l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else      (op (deep-foldr op nv term  (car l)) (deep-foldr op nv term  (cdr l))))))




(define (cons* a b)
  (if (equal? a '_)
      b
      (cons a b)))

(define (deep-delete-h l atom-depth)
  (cond ((null? l) '() )
        ((atom? l) (if (> atom-depth l)
                       '_ ;; empty string
                       l ) )
        (else      (cons* (deep-delete-h (car l) (+ 1 atom-depth) ) (deep-delete-h (cdr l) atom-depth)))))


(define (deep-delete l) 
  (deep-delete-h l 0))

(define l '(1 (2 (2 4) 1 ) 0 ( 3 (1) ) ))

;;; ==================

