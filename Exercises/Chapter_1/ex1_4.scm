; Exercise 1.4
; functional programming? We can use operators as values in expressions

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))