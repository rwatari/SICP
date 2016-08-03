;Exercise 1.19
(define (fib n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) (* q q))
                     (+ (* 2 p q) (* q q))
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))
#|
one T transform:
a <- a+b
b <- a
p = 0
q = 1
two T transforms:
a <- 2a + b
b <- a + b
p = 1 
q = 1

4 T transforms:
a <- 5a + 3b
b <- 3a + 2b
p = 2
q = 3

a <- bq + aq + ap
b <- bp + aq

T2
a <- bpq + aqq + bqq + aqq + apq + bpq + apq + app
     b(2pq + q2) + a(2pq + q2) + a(q2 + p2)
|#