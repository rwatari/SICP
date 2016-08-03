; Exercise 3.23 Deques
#|
Deque representation is pair with first pointer to front of sequence and 
second pointer to rear of sequence. 

Sequence is pairs with car pointing to a pair containing the item and a pointer
to the previous pair in the sequence

[|][|]
 V  \ - - - V 
[|][-]---->[|][/]
 V  ^----   V
[a][?]  |  [b][|]
        \------/
|#


;;; Constructor
(define (make-deque) (cons '() '()))

;;; Predicate
(define (empty-deque? deque)
  (eq? (front-ptr deque) '()))

;;; Selectors
(define (front-ptr deque)
  (car deque))

(define (rear-ptr deque)
  (cdr deque))
       
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "Empty deque!")
      (caar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "Empty deque!")
      (caar (rear-ptr deque))))

;;; Mutators

(define (front-insert-deque! deque item)
  ;;; inserts item to front of deque
  (let ((new-item (cons (cons item
                              '())
                        '())
                  ))
    ;;; caar of previous front-ptr must point to new-item
    ;;; cdr of new-item must point to previous front-ptr
    (cond ((empty-deque? deque)
           (set-car! deque new-item)
           (set-cdr! deque new-item)
           (print-deque deque))
          (else (set-cdr! (car (front-ptr deque)) new-item)
                (set-cdr! new-item (front-ptr deque))
                (set-car! deque new-item)
                (print-deque deque)))))

(define (rear-insert-deque! deque item)
  ;;; inserts item from the rear of the deque
  (let ((new-item (cons (cons item
                              '())
                        '())
                  ))
    ;;; caar of new-item is the item inserted to deque
    ;;; If adjoining to non-empty deque, cdar of new-item
    ;;; will point to previous rear of deque
    (cond ((empty-deque? deque)
           (set-car! deque new-item)
           (set-cdr! deque new-item)
           (print-deque deque))
          (else (set-cdr! (car new-item) (rear-ptr deque))
                (set-cdr! (rear-ptr deque) new-item)
                (set-cdr! deque new-item)
                (print-deque deque)))))

(define (front-delete-deque! deque)
  ;;; removes front element of deque
  ;;; makes sure to change reverse pointer of the new front to a nil
  (cond ((empty-deque? deque)
         (error "Empty deque!"))
        (else (set-car! deque (cdr (front-ptr deque)))
              (set-cdr! (car (front-ptr deque)) '())
              (print-deque deque))))

(define (rear-delete-deque! deque)
  ;;; removes rear element of deque
  ;;; first sets new rear-ptr then changes the cdr to nil
  (cond ((empty-deque? deque)
         (error "Empty deque!"))
        (else (set-cdr! deque (cdar (rear-ptr deque)))
              (set-cdr! (rear-ptr deque) '())
              (print-deque deque))))

(define (print-deque deque)
  (map car (front-ptr deque)))

;;; shortcuts
(define fi front-insert-deque!)
(define ri rear-insert-deque!)
(define fd front-delete-deque!)
(define rd rear-delete-deque!)