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
;; or cdr that is not a subtree, then to add the item to the result.
(defun fringe (tree)
  (labels ((iter (tree result)
             (cond ((null tree) result)
                   ((not (consp tree)) (cons tree result))
                   (T (iter (car tree) 
                            (iter (cdr tree) result))))))  

    (iter tree nil)))


;; Testing
(defparameter x (list (list 1 2) (list 3 4)))

(equal '(1 2 3 4) (fringe x))

(equal '(1 2 3 4 1 2 3 4) (fringe (list x x)))

;; This exercise proved to be particularly challenging for me.  The key mental
;; leap is that the action in this case needs to occur when we find a leaf.
;; Then and only then, do we need to add the leaf node to the result.  
;; Separating this concept from the tree walking iteration concept took me a
;; number of attempts.  Reviewing section 2.2.2 in the text showed the count-
;; leaves procedure.  Using the structure of that method (with cond, and not
;; if then) is what finally enabled me to get this one correct.  Cond is
;; extrememly useful since it easily lets you have multiple branches in your
;; procedure and clearly separates the actions taken in each.
