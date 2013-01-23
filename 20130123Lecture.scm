(define memeber?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      (else member? a (cdr l)))))

(define member*?
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((eq? a (car l)) #t)
      ((list? (car l)) (or (member*? a (car l)) (member*? a (cdr l))))
      (else (member*? a (cdr l))))))

(define nonumbers*
  (lambda (l)
    (cond
      ((null? l) '())
      ((number? (car l)) (nonumbers* (cdr l)))
      ((pair? (car l)) (cons (nonumbers* (car l)) (nonumbers* (cdr l))))
      (else (cons (car l) (nonumbers* (cdr l)))))))

; remove x from list l whereever it occurs
(define remove**
  (lambda (x l)
    (cond
      ((null? l) '())
      ((eq? x (car l)) (remove** x (cdr l)))
      ((pair? (car l)) (cons (remove** x (car l)) (remove** x (cdr l))))
      (else (cons (car l) (remove** x (cdr l)))))))

;test list equality
(define listeq?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((eq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2)))
      ((and ;Ran into a problem, prof is posting the rest of this method online

;
; Begin in class work
;

;replace first element with second whereever it occurs
(define replace*
  (lambda (a b l)
    (cond
      ((null? l) '())
      ((eq? a (car l)) (cons b (replace* a b (cdr l))))
      ((pair? (car l)) (cons (replace* a b (car l)) (replace* a b (cdr l))))
      (else (cons (car l) (replace* a b (cdr l)))))))

;sum all numbers in list
(define sumnumbers
  (lambda (l)
    (cond
      ((null? l) 0)
      ((number? (car l)) (+ (car l) (sumnumbers (cdr l))))
      ((pair? (car l)) (+ (sumnumbers (car l)) (sumnumbers (cdr l))))
      (else (sumnumbers (cdr l))))))

;remove all sublists, but keep elements
(define flatten
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (append (flatten (car l)) (flatten (cdr l))))
      (else (cons (car l) (flatten (cdr l)))))))

