;;;; 6.001 Fall 1995
;;;; This is the file ps9code.scm

;;; The following definitions install rational arithmetic.  Warning:
;;; Don't use any arithmetic operations other than these.

;(define + (access + '()))
;(define - (access - '()))
;(define * (access * '()))
;(define / (access / '()))

;;; some basic stream operations

;; the empty stream is the same as the empty list in our implementation
;; of streams
(define the-empty-stream '())

(define (stream-map proc stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (proc (stream-car stream))
                   (stream-map proc (stream-cdr stream)))))

(define (add-streams s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (cons-stream (+ (stream-car s1) (stream-car s2))
                      (add-streams (stream-cdr s1)
                                   (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series -1 s))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))

;;; display the first n coefficients of a series

(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-car s))
	     (show-series (stream-cdr s) (- nterms 1)))))

;;; return the coefficient of x^n

(define (series-coeff s n)
  (stream-ref s n))



;;; create a (finite) series from a list of coefficients

(define (coeffs->series list-of-coeffs)
  (define zeros (cons-stream 0 zeros))
  (define (iter list)
    (if (null? list)
        zeros
        (cons-stream (car list)
                     (iter (cdr list)))))
  (iter list-of-coeffs))


;;; create a series from a procedure: nth term is P(n)
;;; requires non-neg-integers to be 0,1,2,3....


(define (proc->series proc)
  (stream-map proc non-neg-integers))


;; Defining basic series
(define ones
  (cons-stream 1 ones))

(define non-neg-integers
  (cons-stream 0
               (add-streams ones
                            non-neg-integers)))


(define alt-ones
  (cons-stream 1
               (cons-stream (- 1)
                            alt-ones)))

#|
(define zeros
  (add-streams alt-ones
               (stream-cdr alt-ones)))
|#

;; Define mul-series
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-series (scale-series (stream-car s2) (stream-cdr s1))
                           (mul-series s1 (stream-cdr s2)))))

;; Inverting a unit power series
;; unit power series has constant term 1
;; The inverse of a unit power series S is X s.t. S * X = 1

(define (invert-unit-series s)
  (cons-stream 1
               (negate-series (mul-series (stream-cdr s)
                                          (invert-unit-series s)))))

;; Inverting series with other constant terms
(define (invert-series s)
  (scale-series (/ 1 (stream-car s))
                (invert-unit-series (scale-series (/ 1 (stream-car s)) s))))

;; Dividing two power series
(define (div-series numer denom)
  (if (= (stream-car denom) 0)
      (error "Error div-series: Denominator has constant 0")
      (mul-series numer
                  (invert-series denom))))



(define sin-series
  (proc->series (lambda (n)
                  (if (even? n)
                      0
                      (if (= (modulo n 4) 1)
                          (/ 1 (factorial n))
                          (- (/ 1 (factorial n))))))))

(define cos-series
  (proc->series (lambda (n)
                  (if (odd? n)
                      0
                      (if (= (modulo n 4) 0)
                          (/ 1 (factorial n))
                          (- (/ 1 (factorial n))))))))

(define factorial-stream
  (cons-stream 1
               (mul-streams factorial-stream
                            (stream-cdr non-neg-integers))))

(define (mul-streams s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (cons-stream (* (stream-car s1) (stream-car s2))
                       (mul-streams (stream-cdr s1)
                                    (stream-cdr s2))))))

(define (factorial n)
  (stream-ref factorial-stream n))

;; Integrating series
(define (integrate-series-tail s)
  (mul-streams s
               (stream-map (lambda (x)
                             (/ 1 x))
                           (stream-cdr non-neg-integers))))

(define exp-series
  (cons-stream 1
               (integrate-series-tail exp-series)))

;; using the integral relations we can write sin and cos with mutually recursive
;; definitions
(define sine-series
  (cons-stream 0
               (integrate-series-tail cosine-series)))

(define cosine-series
  (cons-stream 1
               (negate-series (integrate-series-tail sine-series))))

;; Taking the derivative
(define (derivative-series s)
  (mul-streams (stream-cdr s)
               (stream-cdr non-neg-integers)))

;; defining tan and sec using div-series
(define tangent-series
  (div-series sine-series cosine-series))

(define secant-series
  (invert-series cosine-series))

(define arctan-series
  (integrate-series-tail (invert-series (coeffs->series '(1 0 1)))))

;; Approximation 
(define (approximate taylor-series x0)
  ; Generates a stream of approximations for f(x0) where 
  ; series is the taylor approx of f(x)
  (define powers-of-x0
    (cons-stream 1
                 (scale-series x0 powers-of-x0)))
  (partial-sums (mul-streams powers-of-x0
               taylor-series))
  )

(define (partial-sums s)
  ; returns a stream of the partial sums where stream element n 
  ; is the sum of the first n elements of s
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s)))
  )

(define bernoulli
  (mul-streams (invert-series (stream-cdr exp-series))
               factorial-stream))