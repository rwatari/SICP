(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;;; m-semaphore is a semaphore which is implemented in terms of a mutex
;; The mutex protects the count changing operation from multiple concurrent
;; calls. 
(define (make-m-semaphore n)
  (let ((mutex (make-mutex))
        (count 0))
    (define (the-m-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (cond ((< count n)
                    (set! count (+ count 1))
                    (mutex 'release))
                   (else 
                     (mutex 'release)
                     (the-m-semaphore 'acquire)) ;;repeat until acquire
            ((eq? m 'release)
             (mutex 'acquire)
             (if (> count 0)
                 (set! count (- count 1))
                 (error "m-semaphore: Nothing to release"))
             (mutex 'release))))))

;;; tas-semaphore is a semaphore implemented in terms of atomic test-and-set!
;; operations. 
(define (make-tas-semaphore n)
  (let ((cell (list false))
        (count 0))
    (define (the-tas-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-tas-semaphore 'acquire)
                 (cond ((< count n)
                        (set! count (+ count 1))
                        (clear! cell))
                       (else (the-tas-semaphore 'acquire)
                             (clear! cell)))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-tas-semaphore 'release)
                 (cond ((> count 0)
                        (set! count (- count 1))
                        (clear! cell))
                       (else (clear! cell)))))))))

