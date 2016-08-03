; Ex 1.7

(define (average x y)
  (/ (+ x y) 2))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
(define (sqrt x) (sqrt-iter 1.0 x))

; Massive slow down at x = 10^13
; large error at x= 0.001

; new good-enough should compare (improve guess x) with guess
; try to calculate (improve) only once
(define (sqrt-iter-acc new-guess old-guess x)
  (if (gooder-enough? new-guess old-guess)
      new-guess
      (sqrt-iter-acc (improve new-guess x) new-guess x)))
; new good-enough uses margin of error new-guess/old-guess
(define (accept-error x margin)
  (and (> x (- 1 margin))
       (< x (+ 1 margin))))
(define (gooder-enough? new-guess old-guess)
  (accept-error (/ new-guess old-guess) 0.01))

(define (sqrt-acc x) (sqrt-iter-acc 1.0 x x))
