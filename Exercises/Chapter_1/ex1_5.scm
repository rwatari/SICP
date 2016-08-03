; Ex 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; then evaluate 
(test 0 (p))

; applicative order: evaluate operator and operands then applies procedure
; stuck in infinite loop because of the way p is defined: p calls p

; normal order: expand to primitives then reduce
; (if (=0 0) 0 (p))
; if test evaluates true, so (p) is never called, and the test returns 0.