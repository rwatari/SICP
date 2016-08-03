; Digital Circuit Simulator

(load "queues.scm")

;;; Inverter
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  
  (add-action! input invert-input) 'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;;; And-gate
(define (and-gate in1 in2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal in1) (get-signal in2))))
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))))
  
  (add-action! in1 and-action-procedure)
  (add-action! in2 and-action-procedure)
  'ok)
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((and (or (= s1 0) (= s1 1))
              (or (= s2 0) (= s2 1)))
         0)
        (else (error "Invalid signals" s1 s2))))

;;; Or-gate
(define (or-gate in1 in2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal in1) (get-signal in2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  
  (add-action! in1 or-action-procedure)
  (add-action! in2 or-action-procedure)
  'ok)
(define (logical-or s1 s2)
  (cond ((and (= s1 0) (= s2 0)) 0)
        ((and (or (= s1 0) (= s1 1))
              (or (= s2 0) (= s2 1)))
         1)
        (else (error "Invalid signals" s1 s2))))

;;; Wire
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) acccept-action-procedure!)
            (else (error "Unknown operation: WIRTE" m))))
   
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;;; Agenda
; The agenda is made of time segments, which is a pair
; of type Number, Queue. The number is the time and the 
; queue holds the procedures to be run at that time.
; The structure of the agenda is a table with the head containing
; the current time.

; Constructor and selector for time-segments
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; Constructors and selectors for the agenda
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  
  (define (belongs-before? segments)
    ; checks if the new time-segment needs to go before
    ; the segments in the list
    (or (null? segments) ; In case of end of agenda
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    ; creates a new time-segment
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)
    ; Adds the new time-segment to the appropriate location
    ; in the agenda
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest) ; Also covers case at end of agenda
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda
                       (cons (make-new-time-segment time action)
                             segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda
                           (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (after-delay delay action)
  ; Adds an action procedure to the agenda with a set delay
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  ; Drives the simulation in time
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; Probe
(define (probe name wire)
  ; whenever the signal in the wire changes, makes the wire
  ; print the new signal and current time
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))