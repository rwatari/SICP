;PSET 2
#|
(load "curves.scm")
(load "drawing.scm")
(load "utils.scm")
|#
;Exercise 2.2
(define (vertical-line point length)
  (lambda (t) 
    (make-point (x-of point)
                (+ (y-of point)
                   (* t length)))))

;Exercise 3
(define (reflect-through-y-axis curve)
  (lambda (t)
    (let ((ct (curve t)))
      (make-point
       (- (x-of ct))
       (y-of ct)))))

;Exercise 4
(define (connect-ends curve1 curve2)
  (let ((joint (curve 1)))
    (lambda (t)
      (if (< t (/ 1 2))
          (curve1 (* 2 t))
          ((translate (x-of joint)
                      (y-of joint))
           curve2)))))

;Exercise 6
(define (show-points-gosper window level number-of-points initial-curve)
  ((draw-points-on window number-of-points)
   ((repeated gosperize level) initial-curve)))