; Ex 1.8

(define (average x y)
  (/ (+ x y) 2))
;improve applies newton's method
(define (improve guess x)
  (/ (+ (/ x 
           (square guess))
        (* 2 guess))
     3))

(define (square x) (* x x))


(define (cbrt-iter new-guess old-guess x)
  (if (good-enough? new-guess old-guess)
      new-guess
      (cbrt-iter (improve new-guess x) new-guess x)))

(define (accept-error x margin)
  (and (> x (- 1 margin))
       (< x (+ 1 margin))))
(define (good-enough? new-guess old-guess)
  (accept-error (/ new-guess old-guess) 0.01))

(define (cbrt x) (cbrt-iter 1.0 x x))
(define (cube x) (* x x x))
