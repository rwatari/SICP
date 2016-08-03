; Huffman encoding trees

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;; Exercise 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
; create encode-symbol which takes a symbol and a tree and returns the bits
; that encodes that symbol

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  ;;; looks at symbols in left branch
  ;;; if symbol in left branch, go left, (cons '1 rest-of-bits)
  ;;; if leaf return (symbol-leaf)
  (let ((l-branch (left-branch tree))
        (r-branch (right-branch tree)))
    (cond ((equal? (list symbol) (symbols l-branch)) '(0))
          ((equal? (list symbol) (symbols r-branch)) '(1))
          ((element-of-set? symbol (symbols l-branch))
           (cons 0 (encode-symbol symbol l-branch)))
          ((element-of-set? symbol (symbols r-branch))
           (cons 1 (encode-symbol symbol r-branch)))
          (else (error "Symbol not in tree")))))

;;; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
; define successive-merge which takes the ordered set of pairs to generate a tree
(define (successive-merge leaf-set)
; (make-code-tree smallest 2nd-smallest)
; needs a temp merged list and remaining leaves list
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-set)
                                   (cadr leaf-set))
                   (cddr leaf-set)))))

;;; Exercise 2.70
(define rock-tree
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1)
                                 (BOOM 1) (JOB 2) (NA 16) (YIP 9))))
(define rock-song
  (encode '(GET A JOB
                SHA NA NA NA NA NA NA NA NA
                GET A JOB
                SHA NA NA NA NA NA NA NA NA
                WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                SHA BOOM) rock-tree))