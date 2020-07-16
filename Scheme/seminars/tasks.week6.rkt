#lang racket

(define m '((1 2 3) (4 5 6) (7 8 9))) 


;;; Task1
(define (main-diag m)
  (if (or (null? m) (null? (car m)))
      '()
      (cons (caar m) (main-diag (map cdr  (cdr m))))))


;;; Task2

(define (map-matrix f m)
  (if (or (null? m) (null? (car m)))
      '()
      (cons (map f (car m)) (map-matrix f (cdr m)))))
