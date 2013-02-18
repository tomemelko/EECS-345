; apply: takes a function and a list and applies the function to the list
; (apply + '(1 2 3)) ==> 6
(define myapply
  (lambda (f l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (car l))
      (else (f (car l) (myapply f (cdr l)))))))

;sum: takes a list and sums the values in the list
;(define sum
;  (lambda (l)
;    (myapply + l)))

(define apply-f
  (lambda (f)
    (lambda (l)
      (myapply f l))))
;We can redifine sum to be (define sum (apply-f +))

(define factoial-accum
  (lambda (n total)
    (if (zero? n)
        total
        (factorial-accum (- n 1) (* total n)))))

(define factorial-cps
  (lambda (n k)
    (if (zero? n)
        (k 1)
        (factorial-cps (- n 1) (lambda (v) (k (* v n)))))))

(define len
  (lambda (l)
    (if (null? l)
        0
        (+ l (len (cdr l))))))

(define len-cps
  (lambda (l k)
    (if

(define member?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else (member? a (cdr l))))))

(define member-cps?
  (lambda (a l k)
    (cond
      ((null? l) (k #f))
      ((eq? a (car l)) (k #t))
      (else (member-cps? a (cdr l) k)))))

(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))

;Is not corrent right now
(define set-cps?
  (lambda (l k)
    (cond
      ((null? l) (k #t))
      (else (set-cps?