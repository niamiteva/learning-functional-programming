;constructor
(define (make-date d m y)
  (list d m y))

;getters
(define (day date) (car date))
(define (month date) (cadr date))
(define (year date) (caddr date))

(define (day->string date)
  (number->string (day date)))
(define (month->string date)
  (number->string (month date)))
(define (year->string date)
  (number->string (year date)))

;validation
(define (long-month? m)
  (or (= m 1) (= m 3) (= m 5) (= m 7) (= m 8) (= m 10) (= m 12)))

(define (leap? y)
  (or (and (= (remainder y 4) 0) (not (= (remainder y 100) 0)))
      (= (remainder y 400) 0)))

(define (day? d m y)
  (cond ((long-month? m)
         (and (> d 0) (< d 32)))
        ((and (not (long-month? m))(not (= m 2)))
         (and (> d 0) (< d 31)))
        ((= m 2)
         (if (leap? y)
             (and (> d 0) (< d 30))
             (and (> d 0) (< d 29))))
        (else #f)))

(define (month? m)
  (and (> m 0) (< m 13)))

(define (date? date)
  (and (day? (day date) (month date) (year date))
       (month? (month date))))

;working with dates
;print
(define (date->string date)
  (if (date? date)
      (string-append (day->string date) "."
                     (month->string date) "."
                     (year->string date))
      "invalid date"))

(define (add-days d m y days max)
  (cond ((and (> (+ days d) max) (< m 12))
                (list days (+ 1 m) y))
               ((and (> (+ days d) max) (= m 12))
                (list days 1 (+ 1 y)))
               (else (list (+ days d) m y))))

(define (next-day date)
  (if (not (date? date))
      "invalid date")
  
  (define d (day date))
  (define m (month date))
  (define y (year date))
  
  (cond ((long-month? m)
         (add-days d m y 1 31))
        ((and (not (long-month? m)) (not (= m 2)))
         (add-days d m y 1 30))
        (else
         (if (leap? y)
             (add-days d m y 1 29)
             (add-days d m y 1 28)))))

(define (date< date1 date2)
  (if (and (date? date1) (date? date2))
      (if (= (year date1) (year date2))
          (if (= (month date1) (month date2))
              (< (day date1) (day date2))
              (< (month date1) (month date2)))
          (< (year date1) (year date2)))
      "one of the given dates is invalid"))

;JDN = (1461 × (Y + 4800 + (M - 14)/12))/4 +(367 × (M - 2 - 12 × ((M - 14)/12)))/12 - (3 × ((Y + 4900 + (M - 14)/12)/100))/4 + D - 32075
(define (date->jdn date)
  (let ((d (day date))
        (m (month date))
        (y (year date)))
    (- (+ (quotient (* 1461 (+ y 4800 (quotient (- m 14) 12))) 4)
          (quotient (* 367 (- m 2 (* 12 (quotient (- m 14) 12)))) 12)
          d)
       (quotient (* 3 (quotient (+ y 4900 (quotient (- m 14) 12)) 100)) 4)
       32075)))

;something wrong with this formula?
(define (jdn->date jdn)
  (define e (+ (* (- (+ jdn 1401 (quotient (* (quotient (* 4 jdn 274277) 146097) 3) 4)) 38) 4) 3))
  (define h (+ (* 5 (quotient (remainder (+ (* (- (+ jdn 1401 (quotient (* (quotient (* 4 jdn 274277) 146097) 3) 4)) 38) 4) 3) 1461) 4)) 2))
  (make-date (+ (quotient (remainder h 153) 5) 1)
             (+ (remainder (+ (quotient h 153) 2) 12) 1)
             (+ (- (quotient e 1461) 4716)
                (quotient (- 16 (+ (remainder (+ (quotient h 153) 2) 12) 1)) 12))))

(define (day-name num)
  (case num
    ((1) 'Monday)
    ((2) 'Tuesday)
    ((3) 'Wednesday)
    ((4) 'Thursday)
    ((5) 'Friday)
    ((6) 'Saturday)
    ((7) 'Sunday)))

(define (day-num name)
  (cond ((eq? name 'Monday) 1)
        ((eq? name 'Tuesday) 2)
        ((eq? name 'Wednesday) 3)
        ((eq? name 'Thursday) 4)
        ((eq? name 'Friday) 5)
        ((eq? name 'Saturday) 6)
        ((eq? name 'Sunday) 7)))

(define days
  '((Monday 1) (Tuesday 2) (Wednesday 3) (Thursday 4) (Friday 5) (Saturday 6) (Sunday 7)))

(define (weekday date)
  (define dow (+ 1 (remainder (date->jdn date) 7)))
  (day-name dow))

(define (next-weekday day date)
  (define current-day (weekday date))
  (cond ((= (day-num day) (day-num current-day))
         (jdn->date (+ (date->jdn date) 7)))
        ((< (day-num day) (day-num current-day))
         (jdn->date (+ (date->jdn date) (- (day-num current-day) (day-num day)))))
        (else 
         (jdn->date (+ (date->jdn date) (- (day-num day) (day-num current-day)))))))

;events
(define (date= date1 date2)
  (and (= (day date1) (day date2))
       (= (month date1) (month date2))
       (= (year date1) (year date2))))

;check for not a memebr 
(define (events-for-day date events)
  (if (null? events)
      '()
      (if (date= date (caar events))
          (cons (car events) (events-for-day date (cdr events)))
          (events-for-day date (cdr events)))))

(define (filter p l)
  (cond ((null? l) '())
        ((p (car l))
         (cons (car l)
               (filter p (cdr l))))
        (else (filter p (cdr l)))))

;modify sort to combine da events for a date
(define (qsort l)
  (if (null? l)
      '()
      (let ((pivot (car l))
            (others (cdr l)))
        (append (qsort (filter (lambda (x)
                                 (date< (car x) (car pivot)))
                               others))
                (list pivot)
                (qsort (filter (lambda (x)
                                 (or (date= (car x) (car pivot)) (date< (car pivot)(car x))))
                                  others))))))

(define (calendar events)
  (qsort events))


;TESTS
(#%require rackunit rackunit/text-ui)

;chech-equal
(define prob3-tests
  (test-suite "Prob3-tests"
              (test-case "should return the created date"
                         (check-equal? (make-date 1 1 2019)
                                       '(1 1 2019)))
              (test-case "should return the day of date"
                         (check-equal? (day (make-date 12 3 2018))
                                       12))
              (test-case "should return the month of date"
                         (check-equal? (month (make-date 12 3 2018))
                                       3))
              (test-case "should return the year of date"
                         (check-equal? (year (make-date 12 3 2018))
                                       2018))
              (test-case "should return a date converted to string"
                         (check-equal? (date->string (make-date 12 3 2018))
                                       "12.3.2018"))
              (test-case "should check if date is correct before converting it to string"
                         (check-equal? (date->string (make-date 29 13 2019))
                                       "invalid date"))
              (test-case "should return true if a month has 31 days"
                         (check-true (long-month? (month (make-date 12 3 2018)))))
              (test-case "should return false if year is not leap"
                         (check-false (leap? (year (make-date 12 3 2018)))))
              (test-case "should return true if year is leap"
                         (check-true (leap? (year (make-date 12 3 2020)))))
              (test-case "should return false if the created date is not valid"
                         (check-false (date? (make-date 56 3 2018))))
              (test-case "should return false if the created date is not valid"
                         (check-false (date? (make-date 29 2 2019)))
              (test-case "should return false if the created date is not valid"         
                         (check-false (date? (make-date 29 13 2019))))
              (test-case "should return true if the created date is valid"
                         (check-true (date? (make-date 31 12 2019))))
              (test-case "should return true if the day in date is not valid"
                         (check-true (day? 31 12 2019)))
              (test-case "should check if the given date is valid"
                         (check-equal? (date->string (next-day (make-date 29 13 2019)))
                                       "invalid date"))
              (test-case "should return the next day of given date"
                          (check-equal? (date->string (next-day (make-date 21 11 2019)))
                                        "22.11.2019"))
              (test-case "should return the next day of last day of the year"
                         (check-equal? (date->string (next-day (make-date 31 12 2019)))
                                       "1.1.2020"))
              (test-case "should return the next day of last day of a month"
                         (check-equal? (date->string (next-day (make-date 30 11 2019)))
                                       "1.12.2019"))
              (test-case "should check if the given dates for compare a re valid"
                         (check-equal? (date< (make-date 29 13 2019) (make-date 31 12 2019))
                                       "one of the given dates is invalid"))
              (test-case "should return true if a date is less tha another date"
                         (check-true (date< (make-date 29 3 2019) (make-date 31 12 2019))))
              (test-case "should return false if a date is not less tha another date"
                         (check-false (date< (make-date 29 3 2019) (make-date 31 1 2018))))
              (test-case "should return the name of the day"
                         (check-equal? (weekday (make-date 21 11 2019))
                                       'Thursday))
              (test-case "should return the name of the day"
                         (check-equal? (weekday (make-date 13 12 2019))
                                       'Friday))
              (test-case "should return the next date of a given day"
                          (check-equal? (date->string (next-weekday 'Tuesday (make-date 21 11 2019)))
                                        "26.11.2019"))
              (test-case "should return the next date of a given day"
                         (check-equal? (date->string (next-weekday 'Thursday (make-date 21 11 2019)))
                                       "28.11.2019"))
              (test-case "should return the events of given date"
                         (check-equal? (events-for-day (make-date 27 11 2019)
                                                       (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                             (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                             (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
                                       '(((27 11 2019) . "Първа лекция за Хаскел") ((27 11 2019) . "Спират водата в Младост"))))
              (test-case "should order by date the evnets of given calendar"
                         (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                       (cons (make-date 25 12 2019) "Коледа")
                                                       (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                       (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                       '(((23 3 2018) "Концерт на Лепа Брена")((27 11 2019) "Първа лекция за Хаскел" "Спират водата в Младост")((25 12 2019) "Коледа")))))))

(run-tests prob3-tests 'verbose)
