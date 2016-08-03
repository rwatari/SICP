;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (user-says-y?))


(define (user-says-y?) (eq? (read) 'y))

; Problem 2
(define (stop-at n)
  (lambda (your-hand opponent-up-card)
    (< (hand-total your-hand) n)))

; Problem 3
(define (test-strategy strat1 strat2 n-games)
  (define (iter score counter)
    (if (= counter 0)
        score
        (iter (+ score (twenty-one strat1 strat2))
              (- counter 1))))
  (iter 0 n-games))
              
; Problem 4
(define (watch-player strat)
  (lambda (your-hand opponent-up-card)
    (let ((hit (strat your-hand opponent-up-card)))
      (newline)
      (display "Opponent up card: ")
      (display opponent-up-card)
      (newline)
      (display "Your total: ")
      (display (hand-total your-hand))
      (newline)
      (display "Decision: ")
      (if hit
          (display "Hit")
          (display "Stay"))
      (newline)
      hit)))
    
; Problem 5
(define (louis your-hand opponent-up-card)
  (let ((points (hand-total your-hand)))
    (or (< points 12)
        (and (= points 12)
             (< opponent-up-card 4))
        (and (= points 16)
             (< opponent-up-card 10))
        (and (> points 12)
             (< points 16)
             (> opponent-up-card 6)))))

; Problem 6
(define (both strat1 strat2)
  (lambda (your-hand opponent-up-card)
    (and (strat1 your-hand opponent-up-card)
         (strat2 your-hand opponent-up-card))))

; Tut 1
(define (make-card suit number)
  (cons suit number))
(define (get-suit card)
  (car card))
(define (get-number card)
  (cdr card))
(define (make-card-set . cards)
  cards)
(define (new-hand-total hand)
  (if (null? hand)
      0
      (+ (get-number (car hand)) (new-hand-total (cdr hand)))))
(define (new-up-card hand)
  (car hand))
(define (new-hand-add-card hand new-card)
  (append hand (list new-card)))
(define (new-make-hand . cards)
  (make-card-set cards))

; Tut 2
(define (fresh-deck)
  (define (iter deck suit-counter n-counter)
    (if (> n-counter 10)
        deck
        (if (> suit-counter 4)
            (iter deck 1 (+ n-counter 1))
            (iter (append deck (list n-counter))
                  (+ suit-counter 1)
                  n-counter))))
  (iter nil 1 1))

(define (full-fresh-deck)
    (define (iter deck suit-counter n-counter)
    (if (> n-counter 13)
        deck
        (if (> suit-counter 4)
            (iter deck 1 (+ n-counter 1))
            (iter (append deck (list (min 10 n-counter)))
                  (+ suit-counter 1)
                  n-counter))))
  (iter nil 1 1))

(define (append-take-from-front base add)
  (append base (list (car add))))

(define (split-deck cards)
  (let ((l (/ (length cards) 2)))
    (define (splitter half-1 half-2)
      (if (= l (length half-1))
          (cons half-1 half-2)
          (splitter (append-take-from-front half-1 half-2)
                    (cdr half-2))))
    (splitter nil cards)))

(define (riffle-cards card-set-pair)
  (define (riffler deck half-1 half-2)
    (if (null? half-1) 
        deck
        (riffler (append-take-from-front
                  (append-take-from-front deck
                                          half-1)
                  half-2)
                 (cdr half-1)
                 (cdr half-2))))
  (riffler nil (car card-set-pair) (cdr card-set-pair)))

(define (identity t) t)
(define (compose f g)
  (lambda (x)
    (f (g x))))
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (shuffle deck n)
  ((repeated (compose riffle-cards split-deck) n) deck))

(define (split-n cards n)
  (define (splitter half-1 half-2)
    (if (= n (length half-1))
        (cons half-1 half-2)
        (splitter (append-take-from-front half-1 half-2)
                  (cdr half-2))))
  (splitter nil cards))


(define (random-riffle card-set-pair)
  (define (split-rand-cards cards)
    (let ((r (+ (random 5) 1)))
      (if (> r (length cards))
          (cons cards nil)
          (split-n cards r))))
  (define (random-riffler deck half-1 half-2)
    (cond ((and (null? half-1) (null? half-2))
           deck)
          ((null? half-1)
           (append deck half-2))
          ((null? half-2)
           (append deck half-1))
          (else
           (let ((split-1 (split-rand-cards half-1))
                 (split-2 (split-rand-cards half-2)))
             (random-riffler (append
                              (append deck
                                      (car split-1))
                              (car split-2))
                             (cdr split-1)
                             (cdr split-2))))))
  (random-riffler nil (car card-set-pair) (cdr card-set-pair)))

(define (random-shuffle deck n)
  ((repeated (compose random-riffle split-deck) n) deck))
  