(define (member? x l)
  (cond ((null? l)#f)
        ((= x (car l)) #t)
        (else (member? x (cdr l)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (if (zero? n)
      (lambda (x) x)
      (compose f (repeated f (- n 1)))))

(define (findTrace f x k n)
  (if (= k 0)
      '()
      (cons ((repeated f k) x) (findTrace f x (- k 1)))))

(define (takeN l n)
  (if (= n 0)
      '()
      (cons (car l) (takeN (cdr l) (- n 1)))))

(define (count-members l xl)
  (cond ((null? xl) 0)
        ((member? (car xl) l) (+ 1 (count-members l (cdr xl))))
        (else (count-members l (crd xl)))))

(define (max-count-members m l ll)
  (cond ((and (null? ll)
             (= m 0))
        '())
        ((and (null? (car ll) (not (= m 0)))) ll
        ((< m (count-members l (car ll))) (max-count-members (count-members l (car ll)) l (cdr ll)))
        (else (max-count-members m l (cdr ll))))))
     

(define (maxTrace f l k)
  (max-count-members 0 l (map (lambda(x)
                              (take (reverse (findTrace f x k)) (length l)))
                            l)))

