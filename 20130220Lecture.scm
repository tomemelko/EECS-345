;;;;;;;;;;;;;;;;;;
;;From last class
;;;;;;;;;;;;;;;;;;
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

;(define len-cps
;  (lambda (l k)
;    (if

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

(define set-cps?
  (lambda (l k)
    (cond
      ((null? l) (k #t))
      (else (member-cps? (car l) (cdr l) (lambda (v) 
                                           (if v 
                                               (k #f) 
                                               (set-cps? (cdr l) (lambda (v2) (k v2))))))))))

;;;;;;;;;;;;;;;;;;
;;From this class
;;;;;;;;;;;;;;;;;;

(define sum*
  (lambda (l)
    (cond
      ((null? l) 0)
      ((list? (car l)) (+ (sum* (car l)) (sum* (cdr l))))
      ((number? (car l)) (+ (car l) (sum* (cdr l))))
      (else (sum* (cdr l))))))

(define sum-cps*
  (lambda (l k)
    (cond
      ((null? l) (k 0))
      ((list? (car l)) (sum-cps* (car l) 
                                 (lambda (v1) (sum-cps* (cdr l) 
                                                        (lambda (v2) (k (+ v1 v2)))))))
      ((number? (car l)) (sum-cps* (cdr l) (lambda (v) (k (+ v (car l))))))
      (else (sum-cps* (cdr l) k)))))

(define myappend-cps*
  (lambda (l1 l2 k)
    (if (null? (l1))
        (k l2)
        (myappend-cps* (cdr l1) l2 (lambda (v) (k (cons (car l1) v)))))))
      
(define flatten-cps
  (lambda (l k)
    (cond
      ((null? l) (k '()))
      ((null? (car l)) (flatten-cps (cdr l) k))
      ((list? (car l)) (flatten-cps (car l) (lambda (v1) (flatten-cps (cdr l) (lambda (v2) (myappend-cps v1 v2 k))))))
      (else (flatten-cps (cdr l) (lambda (v) (k (cons (car l) v))))))))

(define len*
  (lambda (l)
    (len-cps* l (lambda (v l) (cons v l)))))

(define len-cps*
  (lambda (l k)
    (cond
      ((null? l) (k 0 '()))
      ((list? (car l)) 
       (len-cps* (car l) 
                 (lambda (v1 l1) 
                   (len-cps* (cdr l) 
                             (lambda (v2 l2) 
                               (k (+ 1 v2) 
                                  (cons (cons v1 l1) l2)))))))
      (else (len-cps* (cdr l) (lambda (v l) (k (+ 1 v) l)))))))