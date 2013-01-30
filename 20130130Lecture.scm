(define lookup
  (lambda (var, state)
    (cond
      ((null? state) (error 'undefined))
      ((string-eq? var (car (car state))) (car (cdr (car state))))
      (else (lookup var (cdr state))))))