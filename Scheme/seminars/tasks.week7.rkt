#lang racket

;;; Graphs

(define graph '((1 2 3)
                (2 3)
                (3 4 5)
                (4)
                (5 2 4 6)
                (6 2)))

;; Task 1

(define (count-times el l)
  (if (member el l)
      1
      0))

(define (indegree v g)
  (apply + (map (lambda (row)  (count-times v (cdr row))) g)))

(define (outdegree v g)
  (define needed-v (assoc v g) )
  (length (cdr needed-v)))

;; Task 2

(define (indegree-vertices v g)
  (map car (filter (lambda (row) (member v (cdr row))) g)))

(define (graph-transpose g)
  (map (lambda (row) (cons (car row) (indegree-vertices (car row) g))) g))

;; Task 3

(define (children u g)
  (define l (assoc u g))
  (and l (cdr l)))

(define (all? p? l)
  (equal? l (filter p? l)))

(define (dfs u g path)
   (if (not (null? (children u g)))
       (all? (lambda (v) (and (not (member v path)) (dfs v g (append path (list v))))) (children u g))
       #t))

(define (acyclic? g)
  (all? (lambda (row) (dfs (car row) g (list (car row)))) g))


(define acyclic-graph '((1 2 3)
                        (2 4)
                        (3 6)
                        (4 5)
                        (5)
                        (6)))

;; Task 4

(define (extend path g)
  (define v (car path))
  (map (lambda (u) (cons u path)) (children v g)))

(define (remove-cycles paths)
  (filter (lambda (path) (not (member (car path) (cdr path)))) paths))

(define (extend-paths paths g)
  (apply append (map (lambda (path) (remove-cycles (extend path g))) paths))) 

(define  (bfs u v g)
  (define (bfs-level paths)
    (define next-paths (extend-paths paths g))
    (if (null? paths)
        #f
        (if (member v (map car paths))
            (reverse (assoc v paths))
            (bfs-level next-paths))))
  (bfs-level (list (list u))))

(define (shortest-path g u v)
  (bfs u v g))


;;; Streams

(define-syntax cons-stream
  (syntax-rules () ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

;; Just for easy testing
(define id (lambda (x) x))
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) 
  (if (= n 0)
      id
      (compose f (repeated f (- n 1)))))

;; Task 1

(define ones (cons-stream 1 ones))

;; Task 3

(define (from i)
  (cons-stream i (from (+ i 1))))

;; Task 2

(define nats (from 0))

;; Task 4

(define (fibs-helper a b)
  (cons-stream a (fibs-helper b (+ a b))))

(define fibs (fibs-helper 0 1))


;; Task 5

(define (filter-streams p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-streams p? (tail s)))
      (filter-streams p? (tail s))))

(define (primes-helper l)
  (cons-stream (head l) (primes-helper (filter-streams (lambda (el) (not (= (remainder el (head l)) 0))) (tail l)))))

(define primes  (primes-helper (from 2)))


(define (map-streams f . streams)
  (cons-stream (apply f (map head streams))
               (apply map-streams f (map tail streams))))

;; Task 6

(define (make-list-from-to idx n)
  (define (helper idx)
    (if (= idx n)
        '()
        (cons idx (helper (+ idx 1)))))
  (if (> idx n)
      '()
      (helper idx)))

(define (is-p-triple? a b c)
  (= (+ (sqr a) (sqr b)) (sqr c)))

;(define (triples-to num)
;  (define (helper i)
;    (define j-list (make-list-from-to (+ i 1) num))
;     (if (> i num)
;         '()
;         (cons (cons i (filter (lambda (el) (is-p-triple? i el num)) j-list)) (helper (+ i 1)) )) )
;  (append (apply append (filter (lambda (el) (= (length el) 2) ) (helper 1))) (list num)))

(define (triples-to num)
  (define (helper i)
    (define j-list (make-list-from-to (+ i 1) num))
     (if (> i num)
         '()
         (cons (cons i (filter (lambda (el) (is-p-triple? i el num)) j-list)) (helper (+ i 1)) )) )
  (append (apply append (filter (lambda (el) (= (length el) 2) ) (helper 1))) (list num)))
  ;(map (lambda (el) (append el (list num))) (filter (lambda (el) (= (length el) 2) ) (helper 1)) ))  
(define pythagorean-triples (filter-streams (lambda (el) (>= (length el) 3)) (map-streams triples-to (from 1))))

