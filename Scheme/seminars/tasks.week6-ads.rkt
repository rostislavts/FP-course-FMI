#lang racket

;;; Matrixes

(define m '((1 2 3)
            (4 5 6)
            (7 8 9)))


(define (get-first-col m) (map car m))

(define (remove-first-col m) (map cdr m))

;; Task 1

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (get-first-col m) (transpose (remove-first-col m)))))



(define (vector-sum v1 v2) (map + v1 v2))

(define (vector-mult v1 v2) (apply + (map * v1 v2)))

(define get-first-row car)

;; Task 2

(define (multiply m1 m2)
  (map (lambda (row) (map (lambda (col) (vector-mult row col)) (transpose m2))) m1)) 

;; Task 3

(define (map-matrix f m)
  (map (lambda (row) (map f row)) m))

;; Task 4

(define (main-diagonal m)
 (if (null? m)
     '()
     (cons (caar m) (main-diagonal (map cdr (cdr m))))))

;; Task 5

(define (foldr-matrix op-rows nv-rows op-elems nv-elems m)
  (foldr (lambda (row1 row2) (op-rows row2 (foldr op-elems nv-elems row1))) nv-rows m) )


;;; Trees

(define tree '(1 (3 (4 () ()) ()) (2 (6 (7 () ()) ()) (5 () ()))))

(define (is-leaf? t)
  (and (null? (cadr t)) (null? (caddr t))))

(define get-left cadr)

(define get-right caddr)

(define get-root car)

;; Task 1

(define (collect-pre-order t)
  (if (null? t)
      '()
      (append (list (get-root t))
              (collect-pre-order (get-left t))
              (collect-pre-order (get-right t)))))

(define (collect-in-order t)
  (if (null? t)
      '()
      (append (collect-pre-order (get-left t))
              (list (get-root t))
              (collect-pre-order (get-right t)))))


(define (collect-post-order t)
  (if (null? t)
      '()
      (append (collect-pre-order (get-left t))
              (collect-pre-order (get-right t))
              (list (get-root t)))))


;; Task 2

(define (height t)
  (if (null? t)
      0
      (+ 1 (max (height (get-left t)) (height (get-right t))))))

;; Task 3

(define (depth root t)
  (if (null? t)
      0
      (if (equal? root (get-root t))
          0


          (+ 1 (min (depth root (get-left t)) (depth root (get-right t)) ) ) )) )

(define (level n t)
  (cond
    ((null? t) '())
    ((= n 0) (list (get-root t)))
    (else (append (level (- n 1) (get-left t)) (level (- n 1) (get-right t))))))


;; Task 4

(define (count-leaves t)
  (if (null? t)
      0
      (if (is-leaf? t)
          1
          (+ (count-leaves (get-left t)) (count-leaves (get-right t))))))


;; Task 5

(define (remove-leaves t)
  (if (or (null? t) (is-leaf? t))
      '()
      (list (get-root t) (remove-leaves (get-left t)) (remove-leaves (get-right t)))))


;; Task 6

(define (map-tree f t)
  (if (null? t)
      '()
      (list (f (get-root t)) (map-tree f (get-left t)) (map-tree f (get-right t)))))

;; Task 7

(define (fold-tree f nv t)
  (if (null? t)
      nv
      (f (get-root t) (fold-tree f nv (get-left t)) (fold-tree f nv  (get-right t)))))

;; Task 8

(define (invert t)
  (if (null? t)
      '()
      (list (get-root t) (invert (get-right t)) (invert (get-left t)))))


;; Task 9

(define (bst? t)
  (define (helper curr-t value)
    (if (null? curr-t)
        #t
        (and (or (null? (get-left curr-t))
                 (<= (get-root (get-left curr-t)) value))
             (or (null? (get-right curr-t))
                 (>= (get-root (get-right curr-t)) value))
             (helper (get-left curr-t) (get-root curr-t))
             (helper (get-right curr-t) (get-root curr-t)))))
  (or (null? t) (helper t (get-root t))))

(define bst-tree '(2 (1 (0 () ()) ()) (6 (2 (1 () ()) ()) (10 () ()))))

;; Task 10

(define (insert-bst x t)
  (if (null? t)
      (list x '() '())
      (if (<= x (get-root t))
          (list (get-root t) (insert-bst x (get-left t)) (get-right t))
          (list (get-root t) (get-left t) (insert-bst x (get-right t))))))

; Bonus task
(define (list->bst l)
   (define (helper result curr-l)
     (if (null? curr-l)
         result
         (helper (insert-bst (car curr-l) result) (cdr curr-l))))
  (helper '() l))


;;; Association lists

;; Task 1
(define (index l)
  (define (helper i curr-l)
    (if (null? curr-l)
        '()
         (cons (cons i (car curr-l)) (helper (+ i 1) (cdr curr-l)))))
  (helper 0 l))

;; Task 2

(define (count-times x l)
  (if (null? l)
      0
      (if (= x (car l))
          (+ 1 (count-times x (cdr l)))
          (count-times x (cdr l)))))

(define (histogram l)
  (if (null? l)
      '()
      (cons (cons (car l) (count-times (car l) l))
            (histogram (filter (lambda (el) (not (= (car l) el))) l)))))


;; Task 4

(define (remove-repetitions l)
  (if (null? l)
      '()
      (if (member (car l) (cdr l))
          (cons (car l)
                (remove-repetitions (filter (lambda (el)
                                              (not (equal? (car l) el))) l)))
          (cons (car l) (remove-repetitions (cdr l))))))

;; Task 3

(define (group-by f l)
  (define key-list (remove-repetitions (map f l)))
  (define (helper curr-l keys)
    (if (null? keys)
        '()
        (cons (cons (car keys) (filter (lambda (el) (= (f el) (car keys))) curr-l))
              (helper (filter (lambda (el) (not (= (f el) (car keys)))) curr-l) (cdr keys)))))
  (helper l key-list))

;; Task 5

(define (merge l1 l2 f)
  (if (or (null? l1) (null? l2))
      (if (null? l1)
          l2
          l1)
      (let ((key1 (caar l1))
            (key1-match (filter (lambda (el) (equal? (car el) (caar l1))) l2)))
        (if (null? key1-match)
            (cons (car l1) (merge (cdr l1) l2 f) )
            (cons (cons key1 (foldr f (cdar l1) (map cdr key1-match)))
                  (merge (cdr l1) (filter (lambda (el) (not (equal? (car el) key1))) l2) f))))))


(define l1 '( (1 . 2) (3 . 4) (5 . 6) (7 . 8) ))

(define l2 '( (10 . 2) (3 . 1) (5 . 2) (8 . 8) (9 . 8) ))

(define l3 '())

(define l4 '( (5 . 2) (5 . 3) (2 . 5) (8 . 8) (9 . 8) ))


;; Task 6





