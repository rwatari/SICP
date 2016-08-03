;Exercise 1.18
(define (mult-iter a b)
  (define (double n) (+ n n))
  (define (halve n) (/ n 2))
  (define (even? n)
    (= (remainder n 2) 0))
  (define (iter temp m n)
    (cond ((= n 0) temp)
          ((even? n) (iter temp (double m) (halve n)))
          (else (iter (+ temp m) m (- n 1)))))
  (iter 0 a b))
          