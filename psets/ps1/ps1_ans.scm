;pset 1 answers
;Exercise 2: fix fact
#|
(define fact
  (lambda (n)
    (if (= n 0)
	(* n (fact (- n 1))))))
missing the result for #t in the if statement
|#
(define fact
  (lambda (n)
    (if (= n 0)
        1
	(* n (fact (- n 1))))))

;Exercise 3
(define (comb n k)
  (/ (fact n)
     (* (fact k)
        (fact (- n k)))))

;Exercise 11
(define foo1
  (lambda (x)
    (* x x)))
(define (foo2 x y) (/ x y))
(define (foo3 x)
  (lambda (y) (/ x y)))
(define (foo4 x) (x 3))
(define (foo5 x)
  (cond ((= x 2)
         (lambda () x))
        (else
         (lambda () (* x 3)))))
(define (foo6 x)
  (x (lambda (y) (y y))))