#lang racket

;; Task 1

(define g '((1 2 3 5)
            (2 5)
            (3 4)
            (4 5)
            (5)))

(define g1 '((1 2 3)
            (2 5)
            (3)
            (4 6)
            (5)
            (6)))

(define (children v g)
  (cdr (assoc v g)))

(define (parents v g)
  (map car (filter (lambda (row) (memv v (cdr row))) g)))

(define (all? p? l)
  (foldr (lambda (x y) (and x y)) #t (map p? l)))

(define (any? p? l)
  (foldr (lambda (x y) (or x y)) #f (map p? l)))
;a)

(define (isFamily f g)
  (all? (lambda (u)  (or (and (all? (lambda (w) (memv w f)) (children u g))
                              (not (any? (lambda (w) (memv w f)) (parents u g))))
                         (and (all? (lambda (w) (memv w f)) (parents u g))
                              (not (any? (lambda (w) (memv w f)) (children u g)))))) f))


; b)

;; Strategy
;; all premutations
;; sort by length
;; find first that isFamily

(define (extend result g)
  (define x (map car result))
  (apply append (map (lambda (r) (list (append (parents r g) r)
                         (append (children r g) r))) x)))

(define (minIncluding u g)
  (define (helper result)
    (if (any? (lambda (f) (isFamily f l) ) result) (search .. result)
        call helper with extended result)))




