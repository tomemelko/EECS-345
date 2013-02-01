(define insert
  (lambda (n l)
    (cond
      ((and (> n (car l)) (<= n (car (cdr l))) (cons (car l) (cons n (cdr l)))))
      (else (cons (car l) (insert n (cdr l)))))))