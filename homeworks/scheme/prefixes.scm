(define (prefixes xs)
  (define (remove-last l)
    (if (null? l)
        '()
        (reverse (cdr (reverse l)))))
  (if (null? xs)
      (list '())
      (append (prefixes (remove-last xs)) (list xs))))

;tests 
(define (1+ x) (+ x 1))
(define (square x) (* x x))

(#%require rackunit rackunit/text-ui)

(define prob2-tests
  (test-suite "Prob2-tests"
              (test-case "should return the prefixes of '(1 2 3)"
                         (check-equal? (prefixes '(1 2 3))
                                       '(() (1) (1 2) (1 2 3))))
              (test-case "should return the prefixes of '((1 2) 1 2 3)"
                         (check-equal? (prefixes '((1 2) 1 2 3))
                                       '(() ((1 2)) ((1 2) 1) ((1 2) 1 2) ((1 2) 1 2 3))))
              (test-case  "should return the prefixes of '(#t #f #f)"
                          (check-equal? (prefixes '(#t #f #f))
                                        '(() (#t) (#t #f) (#t #f #f))))
              (test-case  "should return the prefixes of ((1) (1 (2 3) 2 3) 4 (6 5))"
                          (check-equal? (prefixes '((1) (1 (2 3) 2 3) 4 (6 5)))
                                        '(() ((1)) ((1) (1 (2 3) 2 3)) ((1) (1 (2 3) 2 3) 4) ((1) (1 (2 3) 2 3) 4 (6 5)))))
              (test-case  "should return the prefixes of '(1 2 3 4 5)"
                          (check-equal? (prefixes (map square '(1 2 3 4 5)))
                                        '(() (1) (1 4) (1 4 9) (1 4 9 16) (1 4 9 16 25))))
              (test-case  "should return the prefixes of '()"
                          (check-equal? (prefixes '())
                                        '(())))))

(run-tests prob2-tests 'verbose)