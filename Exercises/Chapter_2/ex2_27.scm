; Exercise 2.27

(define (deep-reverse items)
  (define (reverse-store temp items)
    (cond ((null? items) temp)
          ((not (pair? (car items)))
           (reverse-store (cons (car items) temp) (cdr items)))
          (else (reverse-store
                 (cons (reverse-store
                        nil
                        (car items))
                       temp)
                 (cdr items)))))
  (reverse-store nil items))
          