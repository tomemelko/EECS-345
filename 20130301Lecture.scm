(define interpret
  (lambda (file)
    (call/cc (lambda (k)
               (interpret-statement-list
                (parser file)
                (newenvironment) k)))))

(define interpret-statement
  (lambda (statement environment return break continue)
    (cond
      ((eq? (op statement) 'return) (return (eval-expresiion (operand ..))))
      ((eq? (op statement) 'var) (interpret-declare statement environment))
      ((eq? (op statement) '=) (interpret-assign))
      ((eq? (op statement) 'if) (interpret-if statement environment return break continue))
      ((eq? (op statement) 'while) (interpret-while statemtn environment return))
      ((eq? (op statement) 'break) (break environment))
      ;((eq? (op statement) 'continue) (continue environment))
        
(define interpret-while
  (lambda (statement environmet return)
    
    (call/cc (lambda (break) 
    (letrec ((loop (lambda (condition body environment)
                     (if (eval-condition condition environment) 
                         (loop condition body (interpret-statment body environment return))
                     environment))))
      (loop (condition-statement) (body-statement) environment))))