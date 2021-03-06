; Exercise 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
(define (square x) (* x x))
(define tree (list 1 (list 2 (list 3 4))))
