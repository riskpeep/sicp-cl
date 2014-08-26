;Exercise 2.29:
;
;  A binary mobile consists of two branches, a left branch and a right branch. 
;  Each branch is a rod of a certain length, from which hangs either a weight 
;  or another binary mobile. We can represent a binary mobile using compound 
;  data by constructing it from two branches (for example, using list):
;
;  (define (make-mobile left right)
;  (list left right))
;
;  A branch is constructed from a length (which must be a number) together with
;  a structure, which may be either a number (representing a simple weight) or
;  another mobile:
;
;  (define (make-branch length structure)
;  (list length structure))
;
;  a. Write the corresponding selectors left-branch and right-branch, which 
;  return the branches of a mobile, and branch-length and branch-structure, 
;  which return the components of a branch.
;
;  b. Using your selectors, define a procedure total-weight that returns the 
;  total weight of a mobile.
;
;  c. A mobile is said to be balanced if the torque applied by its top-left
;  branch is equal to that applied by its top-right branch (that is, if the 
;  length of the left rod multiplied by the weight hanging from that rod is 
;  equal to the corresponding product for the right side) and if each of the
;  submobiles hanging off its branches is balanced.  Design a predicate that
;  tests whether a binary mobile is balanced.
;
;  d. Suppose we change the representation of mobiles so that the constructors
;  are
;
;  (define (make-mobile left right) (cons left right))
;  (define (make-branch length structure)
;  (cons length structure))
;
;  How much do you need to change your programs to convert to the new 
;  representation?

;; Before we begin, we re-write the procedures given in the problem statement
;; in CL.
;; 
(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

;; A.
;; Using the given definitions, these are simple car and cdr operations.
(defun left-branch (mobile)
  (car mobile))
(defun right-branch (mobile)
  (cadr mobile))                  ; Use cadr to return the value, not a list
(defun branch-length (branch)
  (car branch))
(defun branch-structure (branch)
  (cadr branch))                  ; Use cadr to return the value, not a list

;; Testing
(defparameter a (make-mobile (make-branch 1 1) (make-branch 1 (make-mobile (make-branch 1 (make-mobile (make-branch 1 2) (make-branch 1 2))) (make-branch 1 4)))))
(defparameter b (make-mobile (make-branch 1 1) (make-branch 1 4)))
(left-branch a)
(right-branch a)

(branch-length (left-branch a))
(branch-structure (left-branch a))

(branch-length (left-branch (branch-structure (right-branch a))))
(branch-structure (left-branch (branch-structure  (right-branch a))))

;; B.
;; For total weight, we need a procedure that walks a tree structure and 
;; performs an operation to accumulate the weight on the branches.
(defun total-weight (mobile)
  (labels ((iter (mobile accumulated-weight)
             (cond ((null mobile) accumulated-weight)
                   ((not (consp mobile)) (+ accumulated-weight (branch-structure mobile)))
                   (T (iter (left-branch mobile)
                            (iter (right-branch mobile) accumulated-weight))))))  
    (iter mobile nil)))

(defun total-weight (mobile)
  (+ (if (not (consp (branch-structure (left-branch mobile))))
       (branch-structure (left-branch mobile))
       (total-weight (branch-structure (left-branch mobile))))
     (if (not (consp (branch-structure (right-branch mobile))))
       (branch-structure (right-branch mobile))
       (total-weight (branch-structure (right-branch mobile))))))

;; Testing
(= 9  (total-weight a))
(= 5  (total-weight b))
