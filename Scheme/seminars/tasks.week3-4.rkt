(define (len L)
   (if (null? L)
       0
       (+ 1 (len (cdr L) ) ) ) )


(define (len-i L)
  (define (help result currL)
    (if (null? currL)
        result
        (help (+ 1 result ) (cdr currL) ) ) )
  (help 0 L) )


(define (sum L)
  (if (null? L)
      0
      (+ (car L) (sum (cdr L)) ) ) )

(define (sum-i L)
  (define (help result currL)
    (if (null? currL)
        result
        (help (+ result (car currL) ) (cdr currL) )) )
  (help 0 L) )

(define (last L)
  (if (null? (cdr L) )
      (car L)
      (last (cdr L) )) )


(define (app A B)
  (if (null? A)
       B
       (cons (car A) (app (cdr A) B) ) ))

(define (push x L)
  (app L (list x)))

(define (member? x L)
  (if (null? L)
      #f
      (if (= x (car L) )
          #t
          (member? x (cdr L) ))) )

(define (from-to a b)
  (if (> a b)
      '()
      (cons a (from-to (+ 1 a) b) ) ) )


(define (rev L)
  (if (null? L)
      '()
       (push (car L) (rev (cdr L)) ) ))


(define (my-map f L)
  (if (null? L)
      '()
      (cons (f (car L) ) (my-map f (cdr L) ) ) ) )



(define (my-filter pred? L)
  (if (null? L)
      '()
      (if (pred? (car L) )
          (cons (car L) (my-filter pred? (cdr L) ) )
          (my-filter pred? (cdr L) ) ) ) )


;;; Does not work !!!
(define (partition pred? L)
  (cons (my-filter (not pred?) L)(list(my-filter  (lambda (x) (not pred?) ) L ) ) ) )


(define (scp L)
  (define (prime? n)
    (define (iter i)
      (if (> i (- n 1) )
          #t
          (if (= (remainder n i) 0)
              #f
              (iter (+ i 1))) ) )
    (iter 2))
  (define (help result currL)
    (if (null? currL)
        result
        (if (prime? (car currL) )
            (help (+ result (* (car currL) (car currL) (car currL) ) ) (cdr currL) )
            (help result (cdr currL) )) ) )
  (help 0 L))



(define (take n L)
  (define (iter i currL)
    (if (> i n)
        '()
        (cons (car currL) (iter (+ i 1) (cdr currL) ) ) ))
  (iter 1 L))

(define (drop n L)
  (define (iter i currL)
    (if (null? currL)
        '()
        (if (> i n)
            (cons (car currL) (iter (+ i 1) (cdr currL) ) )
            (iter (+ i 1) (cdr currL) ) ) ))
  (iter 1 L))


(define (my-list-ref L n)
  (define (iter i currL)
    (if (= i n)
        (car currL)
        (iter (+ i 1) (cdr currL) ) ) )
  (iter 0 L))


(define (my-list-tail L n)
  (define (iter i currL)
    (if (= i n)
        (cdr currL)
        (iter (+ i 1) (cdr currL) ) ) )
  (iter 0 L))


(define (insert n x L)
  (app (push x (take n L) ) (drop n L) ))

(define (remove x L)
   (define (getPosition i currL)
     (if (null? currL)
         -1 
         (if (= (car currL) x )
             i
             (getPosition (+ i 1) (cdr currL) ) ) ) )
  (if (= (getPosition 0 L) (- 1) )
      L
      (app (take (- (getPosition 0 L) 1) L) (drop (+ (getPosition 0 L) 1) L) ) ) )


  (define (explode-digits n)
    (define (help currN) 
      (if (= currN 0)
          '()
          (cons (remainder currN 10) (help (quotient currN 10) ) ) ) )
    (rev (help n) ) )

  (define (digit-occurance d n)
    (define L (explode-digits n) )
    (define (iter currL result)
      (if (null? currL)
          result
          (if (= d (car currL) )
              (iter (cdr currL) (+ result 1) )
              (iter (cdr currL) result ) ) ) )
    (iter L 0) )


  (define (remove-repeats L)
    (define (help currL result)
      (if (null? currL)
          result
          (if (and (not (null? (cdr currL)) ) (= (car currL) (car (cdr currL)) ))
              (help (cdr currL) result)
              (help (cdr currL) (push (car currL) result) ) ) ) )
    (help L '() ) )
  
  
  










