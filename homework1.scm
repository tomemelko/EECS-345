;Tom Emelko (tje22)
;All funtions have null checking, for good practice

;Inserts the argument n into ordered list l in the correct spot
(define insert
  (lambda (n l)
    (cond
      ((null? l) '())
      ((and (> n (car l)) (<= n (car (cdr l))) (cons (car l) (cons n (cdr l)))))
      (else (cons (car l) (insert n (cdr l)))))))

;Removes duplicates in the base "layer" of the list
(define removedups
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      ((eq? (car l) (car (cdr l))) (removedups (cdr l)))
      (else (cons (car l) (removedups (cdr l)))))))

;Takes each atom and nests it into a right aligned nest
(define nestlist
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (cons (car l) (cons (nestlist (cdr l)) '()))))))

;Adds the given atom n as the first atom in the list, no matter how "deep"
(define deepcons
  (lambda (n l)
    (cond
      ((null? l) (cons n '()))
      ((list? (car l)) (cons (deepcons n (car l)) (cdr l)))
      (else (cons n l)))))

;Similar to nestlist, but instead is left-aligned, so the first element is the "deepest"
(define nestlistfront
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) l)
      (else (deepcons (cons (car l) '()) (nestlistfront(cdr l)))))))

;Reports the number of parentheses pairs
(define numparens*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((list? (car l)) (+ (numparens* (car l)) (numparens* (cdr l))))
      (else (numparens* (cdr l))))))

;Duplicates each atom and each sublist recursively
(define dup*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (dup* (car l)) (cons (dup* (car l)) (dup* (cdr l)))))
      (else (cons (car l) (cons (car l) (dup* (cdr l))))))))

;Removes duplicate atoms in each sublist
(define removedups*
  (lambda (l)
    (cond
      ((null? l) '())
      ((list? (car l)) (cons (removedups* (car l)) (removedups* (cdr l))))
      ((null? (cdr l)) l)
      ((eq? (car l) (car (cdr l))) (removedups* (cdr l)))
      (else (cons (car l) (removedups* (cdr l)))))))

;Removes duplicates in each sublist, and duplicate sublists
(define removedups**
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (list? (car l)) (null? (cdr l))) (cons (removedups** (car l)) '()))
      ((null? (cdr l)) l)
      ((and 
        (list? (car l)) 
        (list? (car (cdr l)))
        (listeq? (removedups** (car l)) (removedups** (car (cdr l)))))
       (removedups** (cdr l)))
      ((list? (car l)) (cons (removedups** (car l)) (removedups** (cdr l))))
      ((eq? (car l) (car (cdr l))) (removedups** (cdr l)))
      (else (cons (car l) (removedups** (cdr l)))))))

;Transposes the rows and columns of the given matrix
(define transpose
  (lambda (m)
    (cond
      ((null? m) '())
      ((null? (car m)) '())
      (else (cons (map* car m) (transpose (map* cdr m)))))))

;;;;Helper functions;;;;
;My version of the map function, which runs given function f on each element of l
(define map*
  (lambda (f l)
    (cond
      ((null? l) '())
      (else (cons (f (car l)) (map* f (cdr l)))))))

;From Lecture: Return #t if two lists have identical structure and contents
(define listeq?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((eq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2)))
      ((and (pair? (car l1)) (pair? (car l2))) (and (listeq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2))))
      (else #f))))