(define (atom? x)
  (or (not (pair? x))
      (not (null? x))))

(define (findAllChildren t x)
  (cond ((null? (car l)) '())
        ((and (atom? (car l))
              ((= (car l) x))
              (and (not (null? (cadr l)))
                   (not (atom? (cadr l)))))
         (cadr l))
        (else (append (allNumsBetwen (cadr l) x)
                      (list (allNumsBetwen (cddr l) x))))))
        
                                                          
(define (allAtoms l)
  (cond ((null? l) '())
        ((atom? (car l)) (cons (car l) (allAtoms (cdr l))))
        (else (allAtoms (cdr l)))))

(define (hasChild? l x)
  (cond ((null? l) #f)
        ((and (atom? (car l)) (= (car l) x)) #t)
        (else hasChild? (cdr l) x)))

(define (hasParent? t a x)
  (member? a (allAtoms (findAllChildren l x))))
  

(define (allNumsBetween t l x)
  (cond ((null? l) '())
        ((and (atom? (car l)
              (hasParent? t (car l) x)
              (hasChild? (cadr l) x)))
         (car l))
        (else (append (allNumsBetween t (cadr l) x)
                      (allNumsBetween t (cddr l) x)))))
              
                   
(define (findMin l min)
  (cond ((null? l) min)
        ((< (car l) min) (findMin (cdr l) (car l)))
        (else (findMin (cdr l) min))))

(define (numsBetween t x)
  (findMin 1000 (allNumsBetween t t x)))
                 