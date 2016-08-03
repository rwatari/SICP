; Exercise 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define left-branch car)       ; 1st is simple
(define right-branch cadr)     ; Since lists not pairs, need additional car to
(define branch-length car)     ; get rid of nil
(define branch-structure cadr) ;

#| too complicated
(define (total-weight mobile)
  ;;; takes a mobile and returns the total weight (number)
  (define (branch-weight branch)
    ;;; computes the weight of an individual branch
    (let ((structure (branch-structure branch)))
      ; structure is either a weight (number) or another mobile
      (if (not (pair? structure)) ; if not another mobile
          structure               ; return weight
          (total-weight structure)))) ; otherwise recursively find weight of submobile
  (+ (branch-weight (left-branch mobile)) ; add the weights of both sides of the mobile
     (branch-weight (right-branch mobile))))
|# 

(define (total-weight mobile)
  ;;; simple version
  (define (branch-weight branch)
    (total-weight (branch-structure branch)))
  (if (not (pair? mobile))
      mobile
      (+ (branch-weight (left-branch mobile))
         (branch-weight (right-branch mobile)))))

(define (balanced? mobile)
  ;;; takes a mobile and returns boolean if it's balanced
  ;;; balance: torque (branch length* branch weight) is = at every level
  (define (torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (if (not (pair? mobile)) ; if mobile is only weight, trivially balanced
      true
      (and (= (torque (left-branch mobile))   ; if the mobile has branches
              (torque (right-branch mobile))) ; torque must be balnced at this level
           (balanced? (branch-structure (left-branch mobile))) ; and in each branch
           (balanced? (branch-structure (right-branch mobile))))))

(define m
    (make-mobile 
     (make-branch 5 10)
     (make-branch 3
                  (make-mobile
                   (make-branch 1 2)
                   (make-branch 1 2)))))
