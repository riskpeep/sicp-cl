;Exercise 2.22:
;
;  Louis Reasoner tries to rewrite the first squarelist procedure of Exercise 
;  2.21 so that it evolves an iterative process:
;
;  (define (square-list items)
;    (define (iter things answer)
;      (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (square (car things))
;                    answer))))
;    (iter items nil))
;
;  Unfortunately, defining square-list this way produces the answer list in
;  the reverse order of the one desired. Why?  Louis then tries to fix his
;  bug by interchanging the arguments to cons:
;
;  (define (square-list items)
;    (define (iter things answer)
;      (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons answer
;                    (square (car things))))))
;    (iter items nil))
;
;  This doesn’t work either. Explain.

;; An implementation of square
(defun square (x)
  (* x x))

;; We can test this error case empirically by re-writing the first
;; implementation in CL as follows
(defun square-list (items)
  (labels ((iter (things answer)
             (if (null things)
               answer
               (iter (cdr things)
                     (cons (square (car things))
                           answer)))))
  (iter items nil)))

;; Next we test it
(equal '(16 9 4 1) (square-list (list 1 2 3 4)))
;; 
;; We see that the result is indeed reversed.  We can use substitution to 
;; understand why.
;; 
;; (square-list (list 1 2 3 4))
;; (iter (list 1 2 3 4) nil)
;; (iter (iter 2 3 4) 1)
;; (iter (iter 3 4) (4 1))
;; (iter (iter 4) (9 4 1))
;; (iter nil (16 9 4 1))
;; (16 9 4 1)

;; We see that the error occurs in the cons call in the iter procedure.  Cons
;; adds the item to the front of the list, so each successive call to cons
;; inserts the next item to the front of the list and thus the result is a
;; reverse list.

;; Next we examine the alternate implementation.  Again we begin with an 
;; empirical test, by implementing the method in CL and evaluating its result.
(defun square-list-alt (items)
  (labels ((iter (things answer)
             (if (null things)
               answer
               (iter (cdr things)
                     (cons answer
                           (square (car things)))))))
    (iter items nil)))

;; Next we test it
(equal '((((NIL . 1) . 4) . 9) . 16) (square-list-alt (list 1 2 3 4)))
;;
;; We see that the result is now in the proper order, but instead of a list
;; as expected, we have a collection of cons cells.  Since the only change
;; is the cons, we don't need substitution, but instead, we look at the
;; changed cons call.
;; 
;; Cons expects two arguments, the resulting cons cell is the association of
;; the two items.  In order for Lisp to see the result as a list, the second
;; argument must be a list.  In this case, the first argument is a list and
;; the second is a number.  So the resulting cons cell is reverse ordered 
;; (i.e. the cons contains a cons cell on the left side instead of the
;; right side as expected.)
;; 
;; Naturally, this begs the question if there is an method to construct the
;; list as desired.  Some quick google searching yeilds the following.
;; 
;; 1. The general idiom in Lisp is to cons to the front (using cons) and then
;; reverse at the end (using reverse).
;; 
;; 2. CL offers a number of other approaches as well, notably append and nconc.
;; However these suffer from some issues.  Append is known to have performance
;; issues on large lists, and repeated use could significantly impact program
;; performance.  Nconc modifies the input lists, and thus should not be used
;; on lists for which that side effect is undesired (i.e. in most cases)
