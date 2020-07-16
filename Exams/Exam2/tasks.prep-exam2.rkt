#lang racket

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t)
     (cons h (delay t)))))


(define head car)

(define (tail s) (force (cdr s)))

(define (from a)
    (cons-stream a (from (+ a 1))))

(define nats (from 1))

(define ones (cons-stream 1 ones))

(define (map-steam f s)
  (cons-stream (f (head s)) (map-stream f (tail s))))



