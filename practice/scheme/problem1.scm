#lang racket

;problem 1
;graph -> ((children of 1) (children of 2) ...)
(define (family-out-children g tail)
  (if (null? g)
      '()
      (if (null? (car tail))
          (append (+ (index-of g (car tail)) 1) (family-out-children g (cadr tail)))
          (append (family-out-children g (cadr tail))))))

(define (family-out-parents g tail)
  (if (null? g)
      '()
      (if (null? (car tail))
          (append (family-out-parents g (cadr tail)))
          (append (+ (index-of g (car tail)) 1) (family-out-parents g (cadr tail))))))

(define (family-out-parents g)
  )

(define (is-Family f g)
  if(or (null? f) (null? g)
        #f
        (cons ((not (eq? (member f (family-out-children g g)) #f)) #t)
              ((not (eq? (member f (family-out-parents g g)) #f)) #t)
              (else #f))))

(define (min-family flist len res)
  (if (null? flist)
      res
      (if (> len (length (car flist)))
          (min-family (cadr flist) (lenght (car flist)) (car flist))
          (min-family (cadr flist) len res)))) 

(define (minIncluding g)
  (if (null? g)
      "no family"
      (cons ((not (null? (family-out-children g))) (min-family (family-out-children g) 10000 '()))
            ((not (null? (family-out-parents g))) (min-family (family-out-parents g) 10000 '()))
            (else "no family"))))