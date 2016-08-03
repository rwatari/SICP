;;; Tables


(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    ;;; record is key-entry pair
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  ;;; recursively searches for matching key in table
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value) ; If record exists, change value of entry
        (set-cdr! table         ; Otherwise insert key-entry pair
                  (cons (cons key value)
                        (cdr table))))))
(define (make-table)
  (list '*table*))

;;; 2D tables
(define (lookup-2d key-1 key-2 table)
  (let ((subtable
         (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert-2d! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))))