;Exercise 2.35:
;
;  Redefine count-leaves from Section 2.2.2 as an accumulation:
;
;  (define (count-leaves t)
;    (accumulate <??> <??> (map <??> <??>)))

;; Before we begin, we bring in the definition for accumulate
(defun accumulate (op initial sequence)
  (if (null sequence)
    initial
    (funcall op (car sequence)
                (accumulate op initial (cdr sequence)))))

;; From the definition of accumulate, we see that accumulate requires three
;; parameters, op, initial, and sequence.  Since we're counting leaves, we
;; would expect op to be an increment type operator, and initial to be zero
;; to start.  For the map, we can successively count leaves on sequence 
;; elements and set the accumulator to roll up the results of our elements.
;; 
(defun count-leaves (l)
  (accumulate (lambda (x y) (+ x y))
              0
              (map 'list
                   (lambda (x) 
                     (if (atom x)
                       1
                       (count-leaves x)))
                   l)))


;; Testing
;; First, a simple list
(= 4 (count-leaves (list 1 2 3 4)))

;; Next, a nested list
(= 4 (count-leaves (list 1 2 (list 3 4))))

;; And now for something a bit more complex
(= 8 (count-leaves (list 1 2 (list (list 3 4 5) (list 6 7) 8))))

