#lang racket

;; Digits
(define (one . op) (if (null? op) 1 ((car op) 1)))

(define (two . op) (if (null? op) 2 ((car op) 2)))

(define (three . op) (if (null? op) 3 ((car op) 3)))

(define (four . op) (if (null? op) 4 ((car op) 4)))

(define (five . op) (if (null? op) 5 ((car op) 5)))

(define (six . op) (if (null? op) 6 ((car op) 6)))

(define (seven . op) (if (null? op) 7 ((car op) 7)))

(define (eight . op) (if (null? op) 8 ((car op) 8)))

(define (nine . op) (if (null? op) 9 ((car op) 9)))


;; Operations
(define (plus arg2) (lambda (arg1) (+ arg1 arg2)))

(define (minus arg2) (lambda (arg1) (- arg1 arg2)))

(define (times arg2) (lambda (arg1) (* arg1 arg2)))

(define (div arg2) (lambda (arg1) (/ arg1 arg2)))


;; Tests

(require rackunit rackunit/text-ui)

(define fun-with-digits-tests
  (test-suite "Fun with digits tests"
    (test-suite "Just a digit"
      (test-case "Should return 1" (check-eq? (one) 1))
      (test-case "Should return 2" (check-eq? (two) 2))
      (test-case "Should return 3" (check-eq? (three) 3))
      (test-case "Should return 4" (check-eq? (four) 4))
      (test-case "Should return 5" (check-eq? (five) 5))
      (test-case "Should return 6" (check-eq? (six) 6))
      (test-case "Should return 7" (check-eq? (seven) 7))
      (test-case "Should return 8" (check-eq? (eight) 8))
      (test-case "Should return 9" (check-eq? (nine) 9)))
    
    (test-suite "Plus operation"
      (test-case "2 + 1 should return 3" (check-eq? (two (plus (one))) 3))
      (test-case "7 + 3 should return 10" (check-eq? (seven (plus (three))) 10))
      (test-case "5 + 5 should return 10" (check-eq? (five (plus (five))) 10))
      (test-case "1 + 1 should return 2" (check-eq? (one (plus (one))) 2))
      (test-case "8 + 4 should return 12" (check-eq? (eight (plus (four))) 12))
      (test-case "8 + 4 should be equal to 4 + 8" (check-eq? (eight (plus (four))) (four (plus (eight))))))

    (test-suite "Minus operation"
      (test-case "2 - 1 should return 1" (check-eq? (two (minus (one))) 1))
      (test-case "7 - 3 should return 4" (check-eq? (seven (minus (three))) 4))
      (test-case "5 - 5 should return 0" (check-eq? (five (minus (five))) 0))
      (test-case "1 - 1 should return 0" (check-eq? (one (minus (one))) 0))
      (test-case "8 - 4 should return 4" (check-eq? (eight (minus (four))) 4))
      (test-case "4 - 8 should return -4" (check-eq? (four (minus (eight))) (- 4))))

    (test-suite "Times operation"
      (test-case "2 * 1 should return 2" (check-eq? (two (times (one))) 2))
      (test-case "7 * 3 should return 21" (check-eq? (seven (times (three))) 21))
      (test-case "5 * 5 should return 25" (check-eq? (five (times (five))) 25))
      (test-case "1 * 1 should return 1" (check-eq? (one (times (one))) 1))
      (test-case "8 * 4 should return 32" (check-eq? (eight (times (four))) 32)))

    (test-suite "Div operation"
      (test-case "2 / 1 should return 2" (check-eq? (two (div (one))) 2))
      (test-case "6 / 3 should return 2" (check-eq? (six (div (three))) 2))
      (test-case "5 / 5 should return 1" (check-eq? (five (div (five))) 1))
      (test-case "1 / 1 should return 1" (check-eq? (one (div (one))) 1))
      (test-case "8 / 4 should return 2" (check-eq? (eight (div (four))) 2))
      (test-case "3 / 2 should return 1.5" (check-eqv? (three (div (two))) (/ 3 2))))
    
    (test-suite "Random expressions"
      (test-case "(2 + 3 - 5) should return 0" (check-eq? (two (plus (three (minus (five))))) 0))
      (test-case "(2 * (3 + 1)) should return 8" (check-eq? (two (times (three (plus (one))))) 8))
      (test-case "5 / (7 - 2) should return 1" (check-eq? (five (div (seven (minus (two))))) 1))
      (test-case "(8 / (2 * (1 + 1))) should return 2" (check-eq? (eight (div (two (times (one (plus (one))))))) 2))
      (test-case "(2 * 3 * 2) should return 12" (check-eq? (two (times (three (times (two))))) 12))) 
  )
)

(run-tests fun-with-digits-tests 'verbose)