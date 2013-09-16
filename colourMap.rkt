#lang racket

;IMPLEMENT A SOLVER FOR X COLOURS IN RACKET: Christopher Brawn 
;A solver for colouring graphs with x colours
;implements welsh-powell algorithm, get sorted list of vertexs with the largest degree. 
;apply colours starting from the largest degree and continue until done.
;takes input list
;creates a list of lists of adjacency values
;cleans up list of duplicates adjancy values. First value is vertex index
;sorts list by number of adjancy values per vertex, more is better.
;colouring starts with the largest adjacency list, the first list of the sorted list
;if there is no colour for initial node make it 1
;else look at adjacent vertexs and colour 2
;if the node has a colour, and equal current vertex colour, increment colour.
;go through list of lists till all vertexs coloured.
;(1 2 3)(2 4)(4 5)
;the first value is the degree vertex ie; (1 2 3) is 1
;take first adjacency list and check if the first node has been coloured
;if so skip and check that adjacency values are not the same as the vertex colour
;if they are the same colour we need to change them to a different colour
;if there is no coloured vertexes colour them
;take the next adjacency list and do the same until complete.

;HOW TO RUN
;Run graph colour with :
;(colour '([num colours] ((X Y) ....))) where X Y are vertexs making edges

;test cases below
; (colour '(3((1 2) (2 3) (1 3) (3 4))))
;'((1 2) (2 3) (3 1) (4 2))

;(colour'(3((1 2) (2 3) (1 3) (4 5))))
;'((1 1) (2 2) (3 3) (4 1) (5 2))

;(colour  '(3 ((1 2) (2 3) (1 3) (4 5) (5 6) (4 6))))
;'((1 1) (2 2) (3 3) (4 1) (5 2) (6 3))

;(colour '(3 ((1 2) (2 3) (1 3) (4 5))))
;'((1 1) (2 2) (3 3) (4 1) (5 2))

;(colour '(2 ((4 7) (4 6) (4 5) (3 8) (3 6) (3 5) (2 5) (2 7) (2 8) (1 8) (1 7) (1 6))))
;'((1 1) (2 1) (3 1) (4 1) (5 2) (6 2) (7 2) (8 2))

;(colour '(2 ((1 2) (1 3) (2 4) (3 4) (2 5) (3 5) (2 6))))
;'((1 2) (2 1) (3 1) (4 2) (5 2) (6 2))

;(colour '(3 ((1 2) (1 3) (1 5) (1 6) (2 3) (2 6) (2 4) (3 4) (3 5) (4 5) (4 6) (5 6))))
;'((1 1) (2 2) (3 3) (4 1) (5 2) (6 3))

;(colour '(2 ((1 2) (1 3) (1 5) (1 6) (2 3) (2 6) (2 4) (3 4) (3 5) (4 5) (4 6) (5 6))))
;"cannot colour graph with so few colours"

;custom comparator for the list sorting
(define (compare-h v1 v2)
  (cond
    ((eqv? (first v1) (first v2)) 0)
    ((> (first v1) (first v2)) #t)  
    ((< (first v1) (first v2)) #f)))

(define (get-colour lst)
  (car lst)
  )
;returns a list of lists containing vertex adjacency values
(define (get-lists value lst master)
  (cond ((empty? lst) (cons (length master)(cons value master)))
        ((eq? (first (car lst)) value)  (get-lists value (cdr lst) (cons (second (car lst)) master)))                                                  
        (else
         (get-lists value (cdr lst) master)
         )
        )
  )

;get list of lists of adjacency values
(define (get-set-of-lists lst master)
    (cond ((empty? lst) master)
          (else
           (get-set-of-lists (cdr lst) (cons (get-lists (first (first lst)) lst '()) master))
           )
          )
  )

;sort adjacency list based on vertex degree
(define (sort-list lst)
  (sort lst compare-h)
  )

;remove vertex degree after sort
(define (remove-first lst master)
  (cond ((empty? lst) (reverse master))
        (else
         (remove-first (cdr lst) (cons (rest (car lst)) master))
         )
        )
  )

;remove duplicate values from list with first match
(define (remove-duplicates lst match)
  (cond ((empty? lst) '())
        ((eq? (first (car lst)) match) (remove-duplicates (cdr lst) match))
        (else
         (cons (first lst) (remove-duplicates (cdr lst) match))
         )
        )
  )

;clean up list
;'((1 3 2) (2 3) (1 3) (4 5))
(define (clean-list lst)
  (cond ((empty? lst) '())
        (else
         (cons (car lst) (clean-list (remove-duplicates (cdr lst) (first (first lst)))))
         )
        )
  )
  

;(test-list-making '((1 2) (2 3) (1 3) (4 5)))
(define (test-list-making lst)
  (remove-first (sort-list (get-set-of-lists lst '())) '())
  )

; Flat function
; Takes a list
; checks if empty->empty set
; is the first element a list itself? if so, flatten first element, then rest.
; else take head and cons to recursion of tail through flat
(define (flat list)
  (cond ((null? list) ;base case return empty set
         '())
        ((not(pair? list)) ;if not a pair then a list
         list)
        ((list? (car list)) ;if first item a list, recurse on the first item through flat then append to rest
                (append (flat (car list)) (flat (cdr list))))
        (else
         (cons (car list) (flat (cdr list)))))) ;put on first item on front and flatten rest of list

;get largest vertex #
(define (largest-vertex lst)
  (car (sort (flat lst) >))
  )

;(flip-tuples '((1 2) (3 1)) 1)
;((1 2) (1 3))
(define (flip-tuples lst num)
  (cond ((empty? lst) '())
        ((eq? (second (car lst)) num)
         (begin
           ;(print (second (car lst)))
           ;(newline)
         (cons (list (second (car lst)) (first (car lst))) (flip-tuples (cdr lst) num))
         )
         )
        (else
         (cons (car lst) (flip-tuples (cdr lst) num))
         )
        )
  )

;count number of instances of number in a tuple list of lists
;(count-instances '((1 2) (2 1) (1 3)) 2 0)
;2
(define (count-instances lst num return)
  (cond
    ((empty? lst) return)
    ((or (eq? num (first (car lst))) (eq? num (second (car lst))))
     (count-instances (cdr lst) num (+ return 1))
     )
    (else
     (count-instances (cdr lst) num return)
     )
    )
  )

;(index-list '((1 2) (2 3) (2 1) (3 4)) 1)
;returns an indexed list of numbers starting with 1 up to the largest number in the list of lists
;frequency is the value at the index number in list
;trying to figure how to extract position of largest index
;'(2 3 2 1)
(define (index-list lst count)
  (cond ((eq? (largest-vertex lst) count) (cons (count-instances lst count 0) '()))
  (else
   (cons (count-instances lst count 0) (index-list lst (+ count 1)))
   )
  )
  )

;get index of largest number from list. start 0 pos 1 counter 1 always.
;(find-largest-index '(2 3 2 1) 0 1 1)
;2
(define (find-largest-index lst start pos counter)
  (cond ((empty? lst) pos)
        ((< start (car lst))
         (find-largest-index (cdr lst) (car lst) counter (+ counter 1)))
        (else
         (find-largest-index (cdr lst) start pos (+ counter 1))
         )
        )
  )

;massage the input
(define (smooth-list lst)
  (define big-num (find-largest-index (index-list lst 1) 0 1 1))
  (flip-tuples lst big-num)
  )


;take list from start-list and process elements and colours them
(define (eat-list lst vector colour)
  (cond ((empty? lst) vector)       
        ((and (not (eq? colour (vector-ref vector (- (car lst) 1)))) (not (eq? (vector-ref vector (- (car lst) 1)) 0)))
         (begin
           ;(print lst)
           ;(newline)
           ;(print "colour not 1 and vecref not 0 ")
           ;(newline)
           ;(print (car lst))
           ;(newline)
         (eat-list (cdr lst) vector colour)
         )
         )
        ((and (eq? colour 1) (eq? (vector-ref vector (- (car lst) 1)) 0))
         (begin
           ;(print lst)
           ;(newline)
           ;(print "vecref =0 and colour 1")
           ;(newline)
           ;(print (car lst))
           ;(newline)
           (vector-set! vector (- (car lst) 1) 2)
           ;(print vector)
           ;(newline)
           (eat-list (cdr lst) vector colour)
           )
         )
        ((eq? (vector-ref vector (- (car lst) 1)) 0)
         (begin
           ;(print lst)
           ;(newline)
           ;(print "vecref =0 ")
           ;(newline)
           ;(print (car lst))
           ;(newline)
           (vector-set! vector (- (car lst) 1) 1)
           ;(print vector)
           ;(newline)
           (eat-list (cdr lst) vector colour)
           )
         )
        ((eq? colour (vector-ref vector (- (car lst) 1)))
         (begin
           ;(print lst)
           ;(newline)
           ;(print "color=vecref")
           ;(newline)
           ;(print (car lst))
           ;(newline)
           (vector-set! vector (- (car lst) 1) (+ (vector-ref vector (- (car lst) 1)) 1))
           ;(print vector)
           ;(newline)
           (eat-list (cdr lst) vector colour)
           )
         )
        )
  )

;(start-list (make-vector 6) '((1 6 5 3 2) (2 4 6 3) (3 5 4) (4 6 5) (5 6)))
;starts with a list of lists sorted by adjacency length
;if first node is not coloured, ie 0 in the vector at its numerical pos. make it 1
;then examine the rest of the first list is eat-list function returning the updated vector 
;and recursing on remaining lists until complete.
(define (start-list vector lst)
  (cond ((empty? lst) vector)
        ;is the vector pos 0 then make 1 and examine rest of first list
        ((eq? (vector-ref vector (-(car (car lst)) 1)) 0)    
         (begin
           (vector-set! vector (-(car (car lst)) 1) 1)
           (start-list (eat-list (cdr (car lst)) vector 1) (cdr lst))
           )
         )
        (else
         (start-list (eat-list (cdr (car lst)) vector (vector-ref vector (- (car (car lst)) 1))) (cdr lst))
         )
        )
  )
          

;formats the vertexs and executes colouring
;(colour-graph '((1 2) (2 3) (1 3) (4 5)))   
;(colour-graph '((4 7) (4 6) (4 5) (3 8) (3 6) (3 5) (2 5) (2 7) (2 8) (1 8) (1 7) (1 6)))
;(colour-graph '((1 2) (1 3) (2 4) (3 4) (2 5) (3 5) (2 6)))
;interesting graph
;(colour-graph '((1 2) (1 3) (1 5) (1 6) (2 3) (2 6) (2 4) (3 4) (3 5) (4 5) (4 6) (5 6)))
;'((1 6 5 3 2) (2 4 6 3) (3 5 4) (4 6 5) (5 6))
 ;(colour-graph '((1 2) (2 3) (1 3) (4 5) (5 6) (4 6))) 
(define (colour-graph lst)
  ;try to massage input
  (define smooth (smooth-list lst))
 ; (print smooth)
 ; (newline)
  (define get-list (get-set-of-lists smooth '()))
 ; (print get-list)
 ; (newline)
  (define sorted-list (sort-list get-list))
 ; (print sorted-list)
 ; (newline)
  (define remove-lst (remove-first sorted-list '()))
 ; (print remove-lst)
  (start-list (make-vector (largest-vertex lst)) remove-lst)
  )

;format output
(define (format lst num)
  (cond ((empty? lst) '())
        ((cons (list num (first lst)) (format (cdr lst) (+ num 1))
               )
        )
  )
  )

;Run graph colour with (colour ([num colours] '((X Y) ....)) where X Y are vertexs making edges
;test cases below
;(colour  '(3 ((1 2) (2 3) (1 3) (4 5) (5 6) (4 6))))
;'((1 1) (2 2) (3 3) (4 1) (5 2) (6 3))

;(colour '(3 ((1 2) (2 3) (1 3) (4 5))))
;'((1 1) (2 2) (3 3) (4 1) (5 2))

;(colour '(2 ((4 7) (4 6) (4 5) (3 8) (3 6) (3 5) (2 5) (2 7) (2 8) (1 8) (1 7) (1 6))))
;'((1 1) (2 1) (3 1) (4 1) (5 2) (6 2) (7 2) (8 2))

;(colour '(2 ((1 2) (1 3) (2 4) (3 4) (2 5) (3 5) (2 6))))
;'((1 2) (2 1) (3 1) (4 2) (5 2) (6 2))

;(colour '(3 ((1 2) (1 3) (1 5) (1 6) (2 3) (2 6) (2 4) (3 4) (3 5) (4 5) (4 6) (5 6))))
;'((1 1) (2 2) (3 3) (4 1) (5 2) (6 3))

;(colour '(2 ((1 2) (1 3) (1 5) (1 6) (2 3) (2 6) (2 4) (3 4) (3 5) (4 5) (4 6) (5 6))))
;"cannot colour graph with so few colours"
(define (colour lst)
  (define num (car lst))
  (define output (colour-graph (car(cdr lst))))
  (define large-num (largest-vertex (vector->list output)))
  ;(print output)
  (cond ((> large-num num) (print "cannot colour graph with so few colours"))
        ((print (format (vector->list output) 1)))
        )
  )
  






       
