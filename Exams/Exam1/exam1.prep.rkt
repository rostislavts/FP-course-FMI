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



;; Group B

;; Task 2

(define (add-values t y)
  (if (null? t) '()
      (list (+ (root t) y) (add-values (left t) y) (add-values (right t) y))))

(define (clone t x y)
  (list x (add-values t y) (add-values t y)))


(define (cloningTrees)
  (define (helper t)
    (cons-stream t (helper (clone t 1 1))))
  (helper '(1 () ())))


;; Task 3

(define (is-show? l)
  (and (not (null? l))
       (= (length l) 3)
       (string? (car l))
       (pair? (cadr l))
       (integer? (caddr l))))

(define (calc-final-time show)
  (define h (caadr show))
  (define m (cdadr show))
  (define duration (caddr show))
  (define mins (+ (* h 60) m duration))
  (define new-m (remainder mins 60))
  (define new-h (remainder (quotient mins 60) 24))
  (cons new-h new-m))


(define (isProgram? l)
  (and (all? is-show? l)
         #t)) ;; TODO



;; Graph task 2

(define g '((a b c) (b a c) (c b)))

(define (children u g)
  (if (not (assv u g)) '()
  (cdr (assv u g))))

(define (extend paths g)
  (apply append (map (lambda (path)
                       (map (lambda (u)
                              (cons u path))
                            (children (car path) g)))
                     paths)))

(define (bfs u g)
  (define (bfs-level paths)
    (define next-paths (extend paths g))
    (if (null? paths) #f
        (cons-stream (map reverse paths) (bfs-level next-paths))))
 (bfs-level (list (list u))))

(define  (pathsFrom u g)
  (bfs u g))

;; Task 2

(define g1 '((a b c) (b c) (c b)))

(define (dfs u g path)
  (map (lambda (v) (if (memv v path) (append path (list v))
                       (apply append (dfs v g (append path (list v)))))) (children u g)))

(define (infinitePath g)
  (define v (apply append (map (lambda (u) (dfs u g (list u) ) ) (map car g))))
  (define result (filter (lambda (l) (not (null? l))) v))
  (if (null? result) '()
      (cons-stream (car result) (infinitePath g))))


