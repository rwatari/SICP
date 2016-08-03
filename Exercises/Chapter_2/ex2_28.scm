; Exercise 2.28 (flatten)

(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else 
         (append (fringe (car items)) (fringe (cdr items))))))