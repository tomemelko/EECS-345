(define insert
  (lambda (n l)
    (cond
      ((null? l) '())
      ((and (> n (car l)) (<= n (car (cdr l))) (cons (car l) (cons n (cdr l)))))
      (else (cons (car l) (insert n (cdr l)))))))

(define removedups
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      ((eq? (car l) (car (cdr l))) (removedups (cdr l)))
      (else (cons (car l) (removedups (cdr l)))))))

(define nestlist
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (cons (car l) (cons (nestlist (cdr l)) '()))))))

(define deepcons
  (lambda (n l)
    (cond
      ((null? l) (cons n '()))
      ((list? (car l)) (cons (deepcons n (car l)) (cdr l)))
      (else (cons n l)))))

(define nestlistfront
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (deepcons (cons (car l) '()) (nestlistfront(cdr l)))))))

(define numparens*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((list? (car l)) (+ (numparens* (car l)) (numparens* (cdr l))))
      (else (numparens* (cdr l))))))

(define dup*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (dup* (car l)) (cons (dup* (car l)) (dup* (cdr l)))))
      (else (cons (car l) (cons (car l) (dup* (cdr l))))))))

(define removedups*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (removedups* (car l)) (removedups* (cdr l))))
      ((null? (cdr l)) l)
      ((eq? (car l) (car (cdr l))) (removedups* (cdr l)))
      (else (cons (car l) (removedups* (cdr l)))))))

(define removedups**
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (list? (car l)) (null? (cdr l))) (cons (removedups** (car l)) '()))
      ((null? (cdr l)) l)
      ((and 
        (list? (car l)) 
        (list? (car (cdr l)))
        (equal? (removedups** (car l)) (removedups** (car (cdr l)))))
       (removedups** (cdr l)))
      ((list? (car l)) (cons (removedups** (car l)) (removedups** (cdr l))))
      ((eq? (car l) (car (cdr l))) (removedups** (cdr l)))
      (else (cons (car l) (removedups** (cdr l)))))))

(define transpose
  (lambda (m)
    (cond
      ((null? m) '())
      ((null? (car m)) '())
      (else (cons (map car m) (transpose (map cdr m)))))))
      
