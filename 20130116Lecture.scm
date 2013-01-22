(define len
  (lambda (l)
    (if (null? l)
        0
        (+ 1 (len (cdr l))))))

(define myList '(a b c d))

(len myList)

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

(define factorial
  (lambda (n)
    (cond
      ((zero? n) 1)
      (else (* n (factorial (- n 1)))))))

(define dotproduct
  (lambda (v1 v2)
    (cond
      ((null? v1) 0)
      ((null? v2) 0)
      (else (+ (* (car v1) (car v2)) (dotproduct (cdr v1) (cdr v2)))))))