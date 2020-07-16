#lang racket

(define (prefixes xs)
  (define len (length xs))
  (define (helper idx)
    (if (> idx len)
        '()
        (cons (take xs idx) (helper (+ idx 1)) ) ) )
  (helper 0))

;; Tests


(require rackunit rackunit/text-ui)

(define prefixes-tests
  (test-suite "Prefixes tests"
    (test-suite "Random lists"
      (test-case "Empty list should return '(())" (check-equal? (prefixes '()) '(())))
      (test-case "List with one element works correctly" (check-equal? (prefixes '(1)) '(() (1))))
      (test-case "Non-empty list '(1 2 3) works correctly" (check-equal? (prefixes '(1 2 3)) '(() (1) (1 2) (1 2 3))))
      (test-case "Deep list '(1 (2 3) 4 (5 (6))) works correctly" (check-equal? (prefixes '(1 (2 3) 4 (5 (6)))) '(() (1) (1 (2 3)) (1 (2 3) 4) (1 (2 3) 4 (5 (6)))))))
  )
)

(run-tests prefixes-tests 'verbose)