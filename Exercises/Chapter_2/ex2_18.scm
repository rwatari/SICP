;Exercise 2.18

(define (reverse items)
  (define (reverse-store temp items)
    (if (null? items)
        temp
        (reverse-store (cons (car items) temp) (cdr items))))
  (reverse-store nil items))
