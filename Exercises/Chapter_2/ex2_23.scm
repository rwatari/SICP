; Exercise 2.23

(define (for-each proc list)
  (cond ((null? list)
         (newline))
        (else (proc (car list))
              (for-each proc (cdr list)))))