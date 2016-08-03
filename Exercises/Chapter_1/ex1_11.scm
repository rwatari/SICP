; Exercise 1.11
; recursive version
(define (f-rec n)
  (cond ((< n 3) n)
        (else (+ (f-rec (- n 1)) 
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3)))))))

; iterative version
(define (f-iter n)
  (define (f-counter fn+2 fn+1 fn final)
    (if (= final 0)
        fn
        (f-counter (+ fn+2
                      (* 2 fn+1)
                      (* 3 fn))
                   fn+2
                   fn+1
                   (- final 1))))
  (f-counter 2 1 0 n))