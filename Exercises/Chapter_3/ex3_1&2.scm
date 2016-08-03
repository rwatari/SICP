; Exercises 3.1 & 3.2


(define (make-accumulator base)
  (define (sum input)
    (begin (set! base (+ base input))
           base))
  sum)

(define (make-monitored proc)
  (let ((counter 0))
    (define (monitor m)
      (cond ((eq? m 'how-many-calls?) counter)
            ((eq? m 'reset-count) (begin (set! counter 0)
                                         counter))
            (else (begin (set! counter (+ 1 counter))
                         (proc m)))))
    monitor))