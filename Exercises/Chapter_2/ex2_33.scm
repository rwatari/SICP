; Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (acc-map p sequence)
  (accumulate
   (lambda (x y)
     (cons (p x)
           y))
   nil
   sequence))

(define (acc-append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (acc-length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))  