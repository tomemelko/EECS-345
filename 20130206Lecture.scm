;Write a function to recognize math expr
(define expression?
  (lambda (expr)
    (cond
      ((null? expr) #f)
      ((number? expr) #t)
      ((not (pair? expr)) #f)
      ((or 
        (eq? '+ (car expr)) 
        (eq? '- (car expr)) 
        (eq? '* (car expr)) 
        (eq? '/ (car expr)) 
        (eq? '% (car expr))) 
       (and 
        (expression? (car (cdr expr))) 
        (expression? car (cdr (cdr expr)))))
      (else false))))

(define value
  (lambda (expr)
    (cond
      ((number? expr) expr)
      ((null? (cdr expr)) (value (car expr)))
      ((not (pair? expr)) 
      ((eq? '+ (operator expr)) (+ (value (operand 1 expr)) (value (operand 2 expr))))
      .. do the same for -, *, /, %