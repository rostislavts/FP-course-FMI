#lang racket

;;; Year 2018
;;; Group A

;; Task 1

(define (count-occurances x l)
   (foldr (lambda (el res) (if (eqv? el x) (+ 1 res) res)) 0 l))

(define (remove-repetitions l)
  (cond ((null? l) '())
        ((memv (car l) (cdr l)) (cons (car l) (remove-repetitions
                                 (filter (lambda (el)
                                           (not (eqv? el (car l))))
                                         (cdr l)))))
        (else (cons (car l) (remove-repetitions (cdr l))))))

(define (make-pairs l)
  (define clear-l (remove-repetitions l))
  (map (lambda (el) (cons el (count-occurances el l))) clear-l))

(define (list-to-most-frequent l)
  (define pairs (make-pairs l))
  (define max-value (foldr (lambda (pair res) (max (cdr pair) res)) 0 pairs))
  (map car (filter (lambda (pair) (= (cdr pair) max-value)) pairs)))

(define (search p? l)
  (cond ((null? l) #f)
        ((p? (car l)) (car l))
        (else (search p? (cdr l)))))

(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (most-frequent ll)
  (define frequences-list (map list-to-most-frequent ll))
  (define result (search (lambda (el)
                           ('90all? (lambda (l1) (memv el l1))
                                 (cdr frequences-list)))
                         (car frequences-list)))
  (if (eqv? #f result) 0
      result))

;; Task 2

(define left cadr)

(define right caddr)

(define root car)

(define (is-leaf? t)
  (and (not (null? t))
       (list? t)
       (= (length t) 3)
       (integer? (car t))
       (null? (left t))
       (null? (right t))))

(define (grow t x)
  (cond ((null? t) #f)
        ((is-leaf? t) (list t (list x '() '()) (list x '() '())))
        (else (list (root t) (grow (left t) x) (grow (right t) x)))))

;                  10
;            5 --> 
;      2 --->      10
; 1 -->      6 --> 10
;      3 --> 10    10
;            10

(define tree     '(1 (3 () ()) (2 (6 () ()) (5 () ()))))
(define res-tree '(1 (3 (10 () ()) (10 () ())) (2 (6 (10 () ()) (10 () ())) (5 (10 () ()) (10 () ())))))


(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (growingTrees)
  (define (helper t height)
    (cons-stream t (helper (grow t height) (+ height 1))))
  (helper '(1 () ()) 2))













