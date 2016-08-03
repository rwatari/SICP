;Exercies 1.16
(define (even? n)
  (= (remainder n 2) 0))
(define (square x)
  (* x x))

(define (exp-iter base power)
  (define (iter temp b n)
    (cond ((= n 0) temp)
          ((even? n) (iter temp (square b) (/ n 2)))
          (else (iter (* temp b) b (- n 1)))))
  (iter 1 base power))

(define (exp-rec base power)
  (cond ((= power 0) 1)
        ((even? power) (square (exp-rec base (/ power 2))))
        (else (* base (exp-rec base (- power 1))))))