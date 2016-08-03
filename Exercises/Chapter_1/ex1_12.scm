; Exercise 1.12
(define (pascal row place)
  (if (or (= row place) 
          (= place 1))
      1
      (+ (pascal (- row 1) (- place 1))
         (pascal (- row 1) place))))
                 