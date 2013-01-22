(define repeat
  (lambda (n a)
    (cond
      ((zero? n) '())
      (else (cons a (repeat (- n 1) a))))))

(define cars
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (cars (cdr l)))))))

(define removeall
  (lambda (x l)
    (cond
      ((null? l) '())
      ((eq? x (car l)) (removeall x (cdr l)))
      (else (cons (car l) (removeall x (cdr l)))))))

(define myappend
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      (else (cons (car l1) (myappend (cdr l1) l2))))))

(define replaceall
  (lambda (a b l)
    (cond
      ((null? l) '())
      ((eq? (car l) a) (replaceall a b (cons b (cdr l))))
      (else (cons (car l) (replaceall a b (cdr l)))))))

(define squares
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (* (car l) (car l)) (squares (cdr l)))))))

(define myreverse
  (lambda (l)
    (cond
      ((null? l) '())
      (else (myappend (myreverse (cdr l)) (cons (car l) '()))))))

(define mymap
  (lambda (f l)
    (cond
      ((null? l) '())
      (else (cons (f (car l)) (mymap f (cdr l)))))))
