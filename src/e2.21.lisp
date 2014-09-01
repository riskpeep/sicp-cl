;Exercise 2.21:
;
;  The procedure square-list takes a list of numbers as argument and returns a
;  list of the squares of those numbers.
;
;  (square-list (list 1 2 3 4))
;  (1 4 9 16)
;
;  Here are two different definitions of square-list. Complete both of them by
;  filling in the missing expressions:
;
;  (define (square-list items)
;    (if (null? items)
;      nil
;      (cons <??> <??>)))
;      
;  (define (square-list items)
;    (map <??> <??>))

;; We begin with the first definition.  In this definition, the blanks must
;; be used to build the result list.  We do this by recursively adding the
;; square of the current item to the list created by the result from the
;; square-list of the remainder of the items list.
(defun square-list (items)
  (if (null items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))))

;; Testing
(equal '(1 4 9 16) (square-list (list 1 2 3 4)))
;; 
;; The second implementation is much simpler, since it uses the higher order 
;; procedure map.  Note that in common lisp, we must define the result type
;; in addition to the procedure and the sequence
(defun square-list2 (items)
  (map 'list (lambda (x) (* x x)) items))

;; Testing
(equal '(1 4 9 16) (square-list2 (list 1 2 3 4)))
