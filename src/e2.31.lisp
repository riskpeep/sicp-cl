;Exercise 2.31:
;
;  Abstract your answer to Exercise 2.30 to produce a procedure tree-map with 
;  the property that squaretree could be defined as
;  
;  (define (square-tree tree) (tree-map square tree))

;; The challenge in this problem is to be able to pass in the procedure you
;; want to use on your tree via the map.  In a way it is a check on the 
;; implementation created in e2.30 since it demands that the implementation
;; separates the tree element procedure from the tree walking behavior.

;; From e2.30 we have the following procedure:
(defun square-tree (tree)
  (cond ((eq tree nil)
         nil)
        ((not (consp tree))
         (* tree tree))
        (T
         (cons (square-tree (car tree)) (square-tree (cdr tree))))))

;; To convert this into tree-map, we must pass in the procedure we apply to
;; each element of the tree.  And modify the places where we use the procedure.
(defun tree-map (operation tree)
  (cond ((eq tree nil)
         nil)
        ((not (consp tree))
         (funcall operation tree))
        (T
         (cons (tree-map operation (car tree)) (tree-map operation (cdr tree))))))

;; Next we define the square procedure
(defun square (x)
  (* x x))

;; Finally, we can redefine the procedure in terms of tree-map as given in the
;; problem.
;; Note we change the name here only to avoid a name collision with previous 
;; definitions.
(defun square-tree-3 (tree)
  (tree-map #'square tree))

;; To test, we use the same tree defined in e2.30
(square-tree-3
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7)))
;;  (1 (4 (9 16) 25) (36 49))

