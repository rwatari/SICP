; Continued Fraction exercises

; 2.1
;recursive
(define (cont-frac-r n d k)
  (define (rec index)
    (if (> index k)
        0
        (/ (n index)
           (+ (d index) (rec (+ index 1))))))
  (rec 1))
;iterative
(define (cont-frac-i n d k)
  (define (iter total rev-index)
    (if (= rev-index 0)
        total
        (iter (/ (n rev-index)
                 (+ (d rev-index) total))
              (- rev-index 1))))
  (iter 0 k))
;finding accuracy wrt phi
(define phi (/ (+ 1 (sqrt 5)) 2))
(define (phi-frac k)
  (cont-frac-i (lambda (i) 1)
               (lambda (i) 1)
               k))
(define (accuracy-count cont-frac num tolerance)
  (define (iter counter)
    (if (< (abs (- (cont-frac counter)
                   num))
           tolerance)
        counter
        (iter (+ counter 1))))
  (iter 0))
; 11 terms is enough

; 2.2
(define (estimate-pi k)
  (/ 4.0 (+ (brouncker k) 1)))
(define (square x) (* x x))
(define (brouncker k)
  (cont-frac-i (lambda (i) (square (- (* 2 i) 1)))
               (lambda (i) 2)
               k))

; 2.3
(define (atan-cf k x)
  (cont-frac-i (lambda (i)
                 (if (= i 1)
                     x
                     (square (* x (- i 1)))))
               (lambda (i)
                 (- (* 2 i) 1))
               k))

; 2.5
(define (nested-acc op r term k)
  (define (iter total counter)
    (if (= counter 0)
        total
        (iter ((op counter) term total) (- counter 1))))
  (iter r k))

(define (nest-sqrt-sum index)
  (lambda (n total)
    (sqrt (+ (n index) total))))

; 2.6 - 10 steps
;
; 2.8

(define (repeated p n)
  (cond ((= n 0) (lambda (x) x))
        ((= n 1) p)
        (else (lambda (x) (p ((repeated p (dec n)) x))))))
(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  ((repeated (lambda (base) (build n d base)) k) b))

; 2.9
(define (r k)
  (lambda (x) (repeated-build k 1 1 x)))

; Optional problem
(define (rec-alg n d k)
  (define (iter a-1 b-1 a0 b0 counter)
    (if (> counter k)
        (/ a0 b0)
        (iter a0
              b0
              (+ (* (d counter) a0)
                 (* (n counter) a-1))
              (+ (* (d counter) b0)
                 (* (n counter) b-1))
              (+ counter 1))))
  (iter 1 0 0 1 1))
        
