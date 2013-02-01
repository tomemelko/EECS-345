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

(define rac
 (lambda (l)
   (cond
     ((null? (cdr l)) (car l))
     (else (rac (cdr l))))))

(define rdc
  (lambda (l)
    (cond
      ((null? (cdr l)) null)
      (else (cons (car l) (rdc (cdr l)))))))

(define snoc
  (lambda (l n)
    (cond
      ((null? l) (cons n '()))
      (else (cons (car l) (snoc (cdr l) n))))))

;(define nestlistfront
;  (lambda (l)
;    (cond
;      ((null? l) '())
;      ((null? (cdr l)) l)
;      (else (snoc (cons (nestlistfront (rdc l)) '()) (rac l))))))

(define nestlistfront
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (deepcons (cons (car l) '()) (nestlistfront(cdr l)))))))