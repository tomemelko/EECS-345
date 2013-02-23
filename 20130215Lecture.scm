(define factorial
  (lambda (n)
    (if (zero? n) 1 (* n (factorial (- n 1))))))

;Tail Recursion
(define tail-factorial
  (lambda (n total)
    (if (zero? n) total (tail-factorial (- n 1) (* n total)))))

;Make it look nice
(define factorial
  (lambda (n)
    (letrec ((tail-factorial 
              (lambda (x total) (if (zero? x) total (tail-factorial (- n 1) (* n total)))))))))