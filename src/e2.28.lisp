;Exercise 2.28:
;
;  Write a procedure fringe that takes as argument a tree (represented as a
;  list) and returns a list whose elements are all the leaves of the tree 
;  arranged in left-to-right order. For example,
;
;  (define x (list (list 1 2) (list 3 4)))
;  (fringe x)
;  (1 2 3 4)
;  (fringe (list x x))
;  (1 2 3 4 1 2 3 4)

;; The challenge in this exercise is to walk the tree, and when we find a car 
;; or cdr that is not a subtree, then to add the item to the result.  To
;; achieve left to right order, ...?

(defun iter2 (tree result)
             (if (null tree)
               result
               (cons (if (consp (car tree))
                       (iter2 (car tree) result)
                       (car tree))
                     (iter2 (cdr tree) result)))
  
  )
(defun fringe (tree)
  (labels ((iter (tree result)
             (if (null tree)
               result
               (cons (if (consp (car tree))
                       (iter (car tree) result)
                       (car tree))
                     (iter (cdr tree) result)))))
  (iter2 tree nil)))

;; Testing
(defparameter x (list (list 1 2) (list 3 4)))

(fringe x)
