;Exercise 2.30: 
;
;  Define a procedure square-tree analogous to the square-list procedure of
;  Exercise 2.21. That is, squaretree should behave as follows:
;
;  (square-tree
;    (list 1
;          (list 2 (list 3 4) 5)
;          (list 6 7)))
;  (1 (4 (9 16) 25) (36 49))
;
;  Define square-tree both directly (i.e., without using any higher-order
;  procedures) and also by using map and recursion.
;
;
;; From e2.21, we have the following implementation of square-list
;; (defun square-list (items)
;;   (if (null items)
;;     nil
;;     (cons (* (car items) (car items)) (square-list (cdr items)))) 

;; To implement square-tree directly, we proceed as follows
(defun square-tree (tree)
  (cond ((eq tree nil)
         nil)
        ((not (consp tree))
         (* tree tree))
        (T
         (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(equal '(1 (4 (9 16) 25) (36 49))
  (square-tree
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7))))
;;  (1 (4 (9 16) 25) (36 49))

;; Using map and recursion, we proceed as follows
(defun square-tree2 (tree)
  (cond ((eq tree nil)
         nil)
        ((not (consp tree))
         (* tree tree))
        (T
         (map 'list #'square-tree2 tree))))

(equal '(1 (4 (9 16) 25) (36 49))
  (square-tree2
    (list 1
          (list 2 (list 3 4) 5)
          (list 6 7))))

;; The solution presented above is unsatisfying as it doesn't really leverage
;; map in a powerful way.
;; 
;; Looking at some other solutions yielded this solution from Eli Bendersky
;; (http://eli.thegreenplace.net/2007/08/10/sicp-section-222/)
;; 
;; (defun square-tree-map (tree)
;;   (mapcar 
;;     (lambda (subtree)
;;       (if (consp subtree)
;;         (square-tree-map subtree)
;;         (square subtree)))
;;     tree))
;;
;; This approach is much more satisfying as an example of map since the
;; entirety of the implementation is encoded in the map lambda.
