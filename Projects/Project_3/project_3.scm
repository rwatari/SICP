;(load "search.scm")
;(load "generate.scm")

(load "~/SICP/Projects/Project_3/search.scm")
(load "~/SICP/Projects/Project_3/generate.scm")

;Exercise 1: make a breadth-first search
;; Same code as DFS-simple but the merge process appends the children of
;; current node to end of search list
(define (BFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))

;Exercise 2: marking visited nodes
;; Implementation of search that can handle cycles by keeping track of 
;; visited nodes
;; 
;; Visited nodes should be kept as argument in internal procedure
(define (filter predicate sequence)
  (cond ((null? sequence)
         '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (search-with-cycles initial-state goal? successors merge graph)
  (define (search-inner-cycles still-to-do visited-nodes)
    ;;; visited-nodes is a list of symbols
    (if (null? still-to-do)
        #f
        (let ((current (car still-to-do)))
          (if *search-debug*
              (begin (display (list 'now-at current))
                     (newline)
                     ;(display (list 'visited visited-nodes))
                     ;(newline)
                     ))
          (if (goal? current)
              current
              (let ((children (successors graph current)))
                ;; filtered-children removes all children that have already
                ;; been visited
                (let ((filtered-children 
                       (filter (lambda (node) (not (memq node visited-nodes)))
                               children)))
                  (search-inner-cycles (merge filtered-children (cdr still-to-do))
                                (cons current visited-nodes))))))))
  (search-inner-cycles (list initial-state) '()))

(define (DFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append new old))
                      graph))

(define (BFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append old new))
                      graph))

;Exercise 3: complete index implementation
;; See search.scm

;Exercise 4: making web index
;; add-document-to-index! modifies an index where the keys are words
;; and the values in the entry are the list of urls which contain that word
;; Generates an entry for each word in the contents of the url
(define (add-document-to-index! index web url)
  (map (lambda (word)
         (add-to-index! index word url))
       (find-node-contents web url)))

    


;Exercise 5: building web crawler
;; search-action is a modification of search-with-cycles allowing
;; for a procedure to be called at each node visited
;; The procedure called in the search function must take the current node 
;; as a variable
(define (search-action initial-state goal? successors merge graph procedure)
  (define (search-inner-cycles still-to-do visited-nodes)
    ;;; visited-nodes is a list of symbols
    (if (null? still-to-do)
        #f
        (let ((current (car still-to-do)))
          (if *search-debug*
              (begin (display (list 'now-at current))
                     (newline)))
          (procedure current) ;; Here's the new procedure
          (if (goal? current)
              #t
              (let ((children (successors graph current)))
                ;; filtered-children removes all children that have already
                ;; been visited
                (let ((filtered-children 
                       (filter (lambda (node) (not (memq node visited-nodes)))
                               children)))
                  (search-inner-cycles (merge filtered-children (cdr still-to-do))
                                (cons current visited-nodes))))))))
  (search-inner-cycles (list initial-state) '()))

(define (BFS-action start goal? graph procedure)
  (search-action start
                 goal?
                 find-node-children
                 (lambda (new old) (append old new))
                 graph
                 procedure))
    
;; make-web-index takes in a web and starting url and returns a
;; procedure that takes a symbol and returns the urls containing that symbol
(define (make-web-index web start-url)
  (let ((web-index (make-index))) ; Initialize web-index
    (BFS-action start-url
                (lambda (url) #f) ; Searches the entire web
                web
                (lambda (url)     ; Adds to index at every node
                  (add-document-to-index! web-index
                                          web
                                          url)))
    (lambda (word)
      (find-in-index web-index word))))
;; Example using make-web-index:                  
;; (define find-documents (make-web-index the-web 'http://sicp.csail.mit.edu/))
;; (find-documents 'HELP)  

;Exercise 6: dynamic web search

;; search-any uses a BFS strategy to find the first url of the given web
;; which contains the given word.
(define (search-any web start-url word)
  (BFS start-url
       (lambda (url)
         (memq word (find-node-contents web url)))
       web))

;; search-all similarly uses a BFS strategy to find all urls whose contents
;; contain the given word
(define (search-all web start-url word)
  (let ((search-results '()))
    (BFS-action start-url
                (lambda (url) #f)
                web
                (lambda (url) ; at all nodes checks if word exists and cons results
                  (if (memq word (find-node-contents web url))
                      (set! search-results (cons url search-results)))))
    search-results))

;Exercise 7: comparing 5&6
;(define random-web (generate-random-web 150))
;; Sort is broken in this implementation of scheme
;; Computer too fast for timed to work :(

;Exercise 8: optimizing index search with vectors
;; optimize-index converts the index structure to an ordered vector.
;; The first element of the vector is the 'index tag, and the index-entries
;; remain as lists. The index-entries are ordered by alphabetical order of the
;; keys.
(define (optimize-index ind)
  ; first convert the index to a vector
  (let ((vect-index (list->vector ind)))
    ; sorting:
    (sort! vect-index
           (lambda (entry1 entry2)
             (cond ((eq? entry1 'index) #t) ;these conditions ensure the index
                   ((eq? entry2 'index) #f) ;tag remains at the beginning of the vector
                   (else (symbol<? (car entry1) (car entry2)))))) ;sorting by keys
    vect-index))

;testing
(define web-index (make-index))
(add-document-to-index! web-index the-web 'http://sicp.csail.mit.edu/)

;Exercise 9: binary search of keys

(define (find-entry-in-optimized-index optind key)
  (define (bin-search lower upper)
    (let ((middle (quotient (+ lower upper) 2)))
      (let ((test-key (car (vector-ref optind middle))))
        (cond ((eq? key test-key)
               (cadr (vector-ref optind middle)))
              ((= lower upper) #f)
              ((symbol<? key test-key)
               (bin-search lower (- middle 1)))
              (else (bin-search (+ middle 1) upper))))))
  (bin-search 1 (vector-length optind)))
    
