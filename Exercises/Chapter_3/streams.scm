#| These functions are already defined in Scheme. 
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

a generalized stream-map:
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))
(define (stream-filter pred stream)
  (cond ((stream-null? stream)
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred
                             (stream-cdr stream)))))

simple implementation of delay
(define (delay exp)
  (lambda () exp))
(define (force delayed-obj)
  (delayed-obj))

delay with memoization
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))
(define (delay exp)
  (memo-proc (lambda () exp)))
|#

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))

(define (divisible? x y)
  (= (remainder x y) 0))

(define (integers-starting-from n)
  (cons-stream n
               (integers-starting-from (+ n 1))))


(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones
  (cons-stream 1
               ones))

(define integers
  (cons-stream 1 
               (add-streams ones integers)))

(define fibs
  (cons-stream
    0
    (cons-stream 1
                 (add-streams (stream-cdr fibs)
                              fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x)
                (* x factor))
              stream))

; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1
               (mul-streams (stream-cdr integers) factorials)))

; Exercise 3.55
;; Partial sums returns s0, s0 + s1, s0 + s1 + s2,...
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))

; Exercise 3.56
;; merge takes two ordered streams and returns the merged ordered stream
;; with no repeats
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car
                                (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car
                                (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))
;; S is the stream which contains only the positive integers with
;; prime factors in {2,3,5}
(define S
  (cons-stream 1
               (merge (scale-stream S 2)
                      (merge (scale-stream S 3)
                             (scale-stream S 5)))))

;1 2 3 4 5 6 8 9 10 12 15 16