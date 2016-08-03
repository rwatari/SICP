; Ex 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;(new-if (= 2 3) 0 5)
; returns 5
;(new-if (= 1 1) 0 5)
; returns 0

(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.01))
(define (square x) (* x x))

(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x) x)))
; what happens?
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

; in new-if, the else causes the value of sqrt-iter-new to be returned, rather than
; an evaluation and return of the value?
