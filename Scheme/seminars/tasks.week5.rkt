#lang racket

(define (atom? x)
  (and (not (null? x)) (not (pair? x)) ))

(define (my-max a b)
  (if (> a b)
      a
      b))

(define (deep-foldr op nv term l)
  (cond ((null? l) nv)
        ((atom? l) (term l))
        (else      (op (deep-foldr op nv term (car l)) (deep-foldr op nv term (cdr l))))))


;;; Task1
(define (maximum l)
  (define (getNV l)
    (if (atom? l)
        l
        (getNV (car l))))
  (and (not (null? l))
  (deep-foldr (lambda (x rec) (if (> x rec) x rec))
              (getNV (car l))
              (lambda (x) x)
              (cdr l))))

(define (snoc x l)
  (append l (list x)) )

(define (deep-filter p? l)
  (deep-foldr append
              '()
              (lambda (x) (if (p? x)
                              (list x)
                              '()))
              l) )

;;; Task2
(define (selection-sort l)
  (if (null? l) l
      (snoc (maximum l)
            (selection-sort (deep-filter (lambda (x)
                                      (not (equal? x (maximum l)) ) )
                                    l) ) ) ))

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l))) ) )

(define (take n l)
  (define (help i currL result)
    (if (> i n)
        result
        (help (+ 1 i) (cdr currL) (snoc (car currL) result) ) ) )
  (help 1 l '()))


(define (drop n l)
  (define (help i currL result)
    (if (<= i n)
        (help (+ 1 i) (cdr currL) result)
        (if (null? currL)
            result
            (help (+ 1 i) (cdr currL) (snoc (car currL) result) ) ) ))
  (help 1 l '()))

;;; Task3
(define (slice a b list)
  (drop (- b a) (take (+ 1 b) list)))


;;; Task4
(define (zip L M)
  (define (help result currL currM)
    (if (null? currL)
        result
        (help (snoc (cons (car currL) (car currM) ) result ) (cdr currL) (cdr currM) ) ))
  (if (not (equal? (length L) (length M)))
      #f
      (help '() L M)) )

;;; Task5
(define (zip-with f L M)
  (define (help result currL currM)
    (if (null? currL)
        result
        (help (snoc (f (car currL) (car currM) ) result ) (cdr currL) (cdr currM) ) ))
  (if (not (equal? (length L) (length M)))
      #f
      (help '() L M)) )


;;; Task6
(define (unique l)
  (define (help currL result)
    (if (null? currL)
        result
        (if (and (not(null? (cdr currL))) (equal? (car currL) (cadr currL) ) )
            (help (cdr currL) result)
            (help (cdr currL) (snoc (car currL) result) )) ) )
  (help l '()))


(define (isInside x l)
  (if (null? l)
      '()
      (if (equal? x (car l) )
          (list x)
          (isInside x (cdr l)) ))  )

;;; Task7
(define (intersection L M)
  (if (null? L)
      L
      (append (isInside (car L) M) (intersection (cdr L) M) ) ))




;;; Task8
(define (union L M)
  (if (null? L)
      M
      (if (equal? (isInside (car L) M) '() ) 
          (append (list (car L)) (union (cdr L) M) )
          (union (cdr L) M))))


;;; Task9
(define (set-minus L M)
  (if (null? L)
      L
      (if (equal? (isInside (car L) M) '() )
          (snoc (car L) (set-minus (cdr L) M ) )
          (set-minus (cdr L) M) )))


;;; Task10
(define (chunk n L)
  (if (null? L)
      L
      (if (< (length L) n)
      (list L)    
      (cons (take n L) (chunk n (drop n L) ) )) ))


(define (resultEl f l)
  (define (getNV l)
    (if (atom? l)
        l
        (getNV (car l))))
  (and (not (null? l))
  (deep-foldr (lambda (x rec) (if (f x rec) x rec))
              (getNV (car l))
              (lambda (x) x)
              (cdr l))))

;;; Task11
(define (selection-sort-l less L)
  (if (null? L)
      '()
      (snoc (resultEl less L)
              (selection-sort-l less (filter (lambda (el)
                                               (not (equal? el (resultEl less L) ) ) )
                                             L)) ) ) )

;;; Task12
(define (count-atoms dl)
  (deep-foldr +
              0
              (lambda (x) 1)
              dl))

;;; Task13
(define  (flatten dl)
  (deep-foldr append
              '()
              (lambda (x) (list x))
              dl))


;;; Task14
(define (deep-reverse dl)
  (deep-foldr snoc
              '()
              (lambda (x) x)
              dl))


;;; Task15

(define (quicksort l)
  )
