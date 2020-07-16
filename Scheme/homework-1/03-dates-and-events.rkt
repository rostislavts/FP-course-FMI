#lang racket

(define (divides? d n)
  (and (not (= d 0)) (= (remainder n d) 0)))

(define (is-leap? year)
  (or (divides? 400 year) (and (divides? 4 year) (not (divides? 100 year)))))

(define (is-valid-month? month)
  (and (>= month 1) (<= month 12)))

(define 31-days-months '(1 3 5 7 8 10 12))

(define (is-valid-day? day month year)
  (and (is-valid-month? month)
       (if (= month 2)
           (if (is-leap? year)
               (and (>= day 1) (<= day 29))
               (and (>= day 1) (<= day 28)))
           (if (member month 31-days-months)
               (and (>= day 1) (<= day 31))
               (and (>= day 1) (<= day 30))))))

(define (make-date day month year)
  (and (is-valid-day? day month year)
       (list 'date  day month year)))

(define day cadr)

(define month caddr)

(define year cadddr)

(define (date? date)
  (and date
       (= (length date) 4)
       (equal? (car date) 'date)
       (integer? (day date))
       (integer? (month date))
       (integer? (year date))
       (is-valid-day? (day date) (month date) (year date))))

(define (date->string date)
  (and (date? date)
       (string-append (number->string (day date)) "."
                      (number->string (month date)) "."
                      (number->string (year date)))))

(define (next-day date)
  (and (date? date)
       (let ((new-date-day-updated   (make-date (+ (day date) 1) (month date)       (year date)))
             (new-date-month-updated (make-date 1                (+ (month date) 1) (year date)))
             (new-date-year-updated  (make-date 1                1                  (+ (year date) 1))))
         (or new-date-day-updated
             new-date-month-updated
             new-date-year-updated))))

(define (date< date1 date2)
  (and (date? date1) (date? date2)
       (or (< (year date1) (year date2))
           (and (= (year date1) (year date2))
                (or (< (month date1) (month date2))
                    (and (= (month date1) (month date2))
                         (< (day date1) (day date2))))))))

(define (get-JDN date)
  (define d (day date))
  (define m (month date))
  (define y (year date))

  (+ (quotient (* 1461 (+ y 4800 (quotient (- m 14) 12))) 4)
     (quotient (* 367 (- m 2 (* 12 (quotient (- m 14) 12)))) 12)
     (- (quotient (* 3 (quotient (+ y 4900 (quotient (- m 14) 12)) 100)) 4))
     d
     (- 32075)))

(define (get-w1 date)
  (+ (remainder (get-JDN date) 7) 1))

(define weekdays '(Monday Tuesday Wednesday Thursday Friday Saturday Sunday))

(define (weekday date)
  (and (date? date) (list-ref weekdays (- (get-w1 date) 1))))

(define (member? x l)
  (if (null? l)
      #f
      (if (equal? x (car l))
          #t
          (member? x (cdr l)))))

(define (is-valid-weekday? weekday)
  (member? weekday weekdays))

(define (next-weekday weekd date)
  (define (helper curr-date)
    (if (equal? (weekday curr-date) weekd)
        curr-date
        (helper (next-day curr-date)))) 
  (and (is-valid-weekday? weekd) (date? date)
       (helper (next-day date))))

(define (is-event? event)
  (and (not (null? event))
       (pair? event)
       (date? (car event))
       (string? (cdr event))))

(define (all? p? l)
  (if (null? l)
      #t
      (if (p? (car l))
          (all? p? (cdr l))
          #f)))

(define (events-for-day date event-list)
  (define (helper ev-list)
    (if (null? ev-list)
           '()
           (if (equal? date (caar ev-list))
               (cons (car event-list) (events-for-day date (cdr event-list)))
               (events-for-day date (cdr event-list)))))  
  (and (all? is-event? event-list)
       (date? date)
       (helper event-list)))

(define (get-dates ev-list)
  (if (null? ev-list)
      '()
      (cons (caar ev-list) (get-dates (cdr ev-list)))))

(define (remove-repetitions date-list)
  (if (null? date-list)
      '()
      (if (null? (cdr date-list))
          date-list
          (if (member? (car date-list) (cdr date-list))
              (cons (car date-list) (remove-repetitions
                                     (filter (lambda (x) (not (equal? x (car date-list))))
                                             (cdr date-list))))
              (cons (car date-list) (remove-repetitions (cdr date-list)))))))

(define (combine daily-events)
  (define (helper d-events)
    (if (null? d-events)
        '()
        (cons (cdr (car d-events)) (helper (cdr d-events)))))
  (cons (caar daily-events) (helper daily-events)))

(define (calendar event-list)
  (define date-list (remove-repetitions (get-dates event-list)))
  (define (helper d-list)
    (if (null? d-list)
        '()
        (cons (combine (events-for-day (car d-list) event-list)) (helper (cdr d-list)))))
  (and (all? is-event? event-list) (helper (sort date-list date<))))


;; Tests


(require rackunit rackunit/text-ui)

(define dates-and-events-tests
  (test-suite "Dates and events tests"
    (test-suite "Make date works correctly"
      (test-case "Wrong day should return false" (check-false (make-date 0 1 1)))
      (test-case "Wrong day should return false" (check-false (make-date -12 1 1)))
      (test-case "Wrong day should return false" (check-false (make-date 32 1 1)))
      (test-case "31-th April should return false" (check-false (make-date 31 4 1)))
      (test-case "29-th Feb of non-leap year should return false" (check-false (make-date 29 2 1)))

      (test-case "Valid day should return date" (check-equal? (make-date 1 1 2000) (list 'date 1 1 2000)))      
      (test-case "Valid day should return date" (check-equal? (make-date 28 2 2019) (list 'date 28 2 2019)))
      (test-case "29-th Feb of leap year should return date" (check-equal? (make-date 29 2 2020) (list 'date 29 2 2020)))
      (test-case "31-th March should return date" (check-equal? (make-date 31 3 2000) (list 'date 31 3 2000)))

      (test-case "Wrong month should return false" (check-false (make-date 1 0 1)))
      (test-case "Wrong month should return false" (check-false (make-date 1 13 1)))
      (test-case "Wrong month should return false" (check-false (make-date 1 -1 1)))

      (test-case "Valid month should return date" (check-equal? (make-date 1 12 2000) (list 'date 1 12 2000)))      
      (test-case "Valid month should return date" (check-equal? (make-date 1 7 2000) (list 'date 1 7 2000)))      
      (test-case "Valid month should return date" (check-equal? (make-date 1 9 2000) (list 'date 1 9 2000)))      

      (test-case "Negative year should return date" (check-equal? (make-date 1 1 -2000) (list 'date 1 1 -2000)))      
      (test-case "0 year should return date" (check-equal? (make-date 1 1 0) (list 'date 1 1 0))))

    (test-suite "Day, month, year work correctly"
      (test-case "Day of 14.12.2019 should return 14" (check-eq? (day (make-date 14 12 2019)) 14))
      (test-case "Month of 14.12.2019 should return 12" (check-eq? (month (make-date 14 12 2019)) 12))
      (test-case "Year of 14.12.2019 should return 2019" (check-eq? (year (make-date 14 12 2019)) 2019)))

    (test-suite "Date check works correctly"
      (test-case "The list (12) should return false" (check-false (date? '(12))))
      (test-case "The list (a b c) should return false" (check-false (date? '(a b c))))
      (test-case "The list (date 14 12 a) should return false" (check-false (date? '(date 14 12 a))))
      (test-case "The list (date a 12 2019) should return false" (check-false (date? '(date a 12 2019))))
      (test-case "The list (date 14 a 2019) should return false" (check-false (date? '(date 14 a 2019))))
      (test-case "The list (14 12 2019) should return false" (check-false (date? '(14 12 2019))))
      (test-case "Valid date structure, but invalid date should return false" (check-false (date? '(date 29 2 2019))))
      
      (test-case "Valid date structure and valid date should return true" (check-true (date? '(date 28 2 2019))))
      (test-case "Valid date using make-date return true" (check-true (date? (make-date 28 2 2019)))))

    (test-suite "Date to string works correctly"
      (test-case "Invalid date should return false" (check-false (date->string (make-date 32 1 2020))))

      (test-case "Valid date (1 1 2020) should return \"1.1.2020\"" (check-equal? (date->string (make-date 1 1 2020)) "1.1.2020")))

    (test-suite "Next day works correctly"
      (test-case "Invalid date should return false" (check-false (next-day (make-date 32 1 2020))))

      (test-case "After 21.11.2019 should be 22.11.2019" (check-equal? (next-day (make-date 21 11 2019)) '(date 22 11 2019)))
      (test-case "After 30.11.2019 should be 1.12.2019" (check-equal? (next-day (make-date 30 11 2019)) '(date 1 12 2019)))
      (test-case "After 31.12.2019 should be 1.1.2020" (check-equal? (next-day (make-date 31 12 2019)) '(date 1 1 2020))))

    (test-suite "Date comparator works correctly"
      (test-case "Invalid dates should return false" (check-false (date< (make-date 32 1 2020) (make-date 1 1 2020))))

      (test-case "Equal dates should return false" (check-false (date< (make-date 1 1 2020) (make-date 1 1 2020))))
      (test-case "Year1 is less than year2 should return true" (check-true (date< (make-date 1 1 2019) (make-date 1 1 2020))))
      (test-case "Year1 is less than year2 should return true regardless of the month" (check-true (date< (make-date 1 12 2019) (make-date 1 1 2020))))
      (test-case "Year1 is less than year2 should return true regardless of the day" (check-true (date< (make-date 12 1 2019) (make-date 1 1 2020))))
      (test-case "Year1 is less than year2 should return true regardless of the day and month" (check-true (date< (make-date 12 12 2019) (make-date 1 1 2020))))
      
      (test-case "Month1 is less than month2 should return true" (check-true (date< (make-date 1 1 2000) (make-date 1 2 2000))))
      (test-case "Month1 is greater than month2 should return false" (check-false (date< (make-date 1 2 2000) (make-date 1 1 2000))))

      (test-case "Day1 is less than day2 should return true" (check-true (date< (make-date 1 1 2000) (make-date 12 1 2000))))
      (test-case "Day1 is greater than day2 should return false" (check-false (date< (make-date 12 1 2000) (make-date 1 1 2000)))))

    (test-suite "Weekday works correctly"
      (test-case "Invalid date should return false" (check-false (weekday (make-date 32 1 2020))))
      
      (test-case "12.9.1999 should return Sunday (lol, my birthday)" (check-equal? (weekday (make-date 12 9 1999)) 'Sunday))
      (test-case "4.12.2019 should return Wednesday" (check-equal? (weekday (make-date 4 12 2019)) 'Wednesday))
      (test-case "7.12.2019 shoud return Saturday" (check-equal? (weekday (make-date 7 12 2019)) 'Saturday)))
 
    (test-suite "Next weekday works correctly"
      (test-case "Invalid date should return false" (check-false (weekday (make-date 32 1 2020))))
      
      (test-case "Next Sunday from 4.12.2019 should be 8.12.2019" (check-equal? (next-weekday 'Sunday (make-date 4 12 2019)) '(date 8 12 2019)))
      (test-case "Next Thursday from 21.11.2019 should be 28.11.2019" (check-equal? (next-weekday 'Thursday (make-date 21 11 2019)) '(date 28 11 2019)))
      (test-case "Next Tuesday from 21.11.2019 should be 26.11.2019" (check-equal? (next-weekday 'Tuesday (make-date 21 11 2019)) '(date 26 11 2019))))

    (test-suite "Events for day works correctly"
      (test-case "Invalid date should return false" (check-false (events-for-day (make-date 32 1 2020)
                (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")))))
      (test-case "Invalid event list should return false" (check-false (events-for-day (make-date 27 11 2019)
                (list (cons (make-date 27 11 2019) 3)
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")))))
     
      (test-case "Valid date and event list should work correctly" (check-equal? (events-for-day (make-date 27 11 2019)
                (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
                                                                                 '(((date 27 11 2019) . "Първа лекция за Хаскел")
                                                                                   ((date 27 11 2019) . "Спират водата в Младост"))))

      (test-case "Valid date and event list should work correctly" (check-equal? (events-for-day (make-date 25 12 2019)
                (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                      (cons (make-date 27 11 2019) "Спират водата в Младост")
                      (cons (make-date 28 11 2019) "Спират водата в Лозенец")
                      (cons (make-date 25 12 2019) "Коледа")
                      (cons (make-date 25 12 2019) "Подаряване на подаръци")
                      (cons (make-date 25 12 2019) "Семейна вечеря")))
                                                                                 '(((date 25 12 2019) . "Коледа")
                                                                                   ((date 25 12 2019) . "Подаряване на подаръци")
                                                                                   ((date 25 12 2019) . "Семейна вечеря")))))

    (test-suite "Calendar works correctly"
      (test-case "Invalid event of event list should return false" (check-false (calendar (list (cons (make-date 27 11 2019) 3)
                (cons (make-date 25 12 2019) "Коледа")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))))

      (test-case "Invalid date of event list should return false" (check-false (calendar (list (cons (make-date -1 11 2019) "Първа лекция за Хаскел")
                (cons (make-date 25 12 2019) "Коледа")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))))

      (test-case "Empty event list should return '()" (check-equal? (calendar '()) '()))
      
      (test-case "Non-sorted event list works correctly" (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                (cons (make-date 25 12 2019) "Коледа")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                  '(((date 23 3 2018) "Концерт на Лепа Брена")
                                    ((date 27 11 2019) "Първа лекция за Хаскел" "Спират водата в Младост")
                                    ((date 25 12 2019) "Коледа"))))

     (test-case "Sorted event list works correctly" (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                (cons (make-date 23 3 2018) "Концерт на Лепа Брена")
                (cons (make-date 27 11 2019) "Спират водата в Младост")
                (cons (make-date 25 12 2019) "Коледа")))
                                  '(((date 23 3 2018) "Концерт на Лепа Брена")
                                    ((date 27 11 2019) "Първа лекция за Хаскел" "Спират водата в Младост")
                                    ((date 25 12 2019) "Коледа")))))   
  )
)

(run-tests dates-and-events-tests 'verbose)