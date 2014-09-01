;Exercise 2.32:
;
;  We can represent a set as a list of distinct elements, and we can represent 
;  the set of all subsets of the set as a list of lists. For example, if the 
;  set is
;
;  (1 2 3),
;
;  then the set of all subsets is
;
;  (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
;
;  Complete the following definition of a procedure that generates the set of
;  subsets of a set and give a clear explanation of why it works:
;  
;  (define (subsets s)
;    (if (null? s)
;      (list nil)
;      (let ((rest (subsets (cdr s))))
;        (append rest (map <??> rest)))))

;; We begin by converting the given procedure to CL.
(defun subsets (s)
  (if (null s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map 'list (lambda (x) (cons (car s) x)) rest)))))

;; Testing
(equal '(NIL (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
  (subsets (list 1 2 3)))

;; Note that in CL, the empty list is equivalent to nil
;; 
;; Now, we discuss why it works.  To begin, we look at the output from trace
(trace subsets)
(subsets (list 1 2 3))
;; We include the results with line numbers to aid our discussion
;;  1. (subsets (list 1 2 3))
;;  2.   0: (SUBSETS (1 2 3))
;;  3.     1: (SUBSETS (2 3))
;;  4.       2: (SUBSETS (3))
;;  5.         3: (SUBSETS NIL)
;;  6.         3: SUBSETS returned (NIL)
;;  7.      2: SUBSETS returned (NIL (3))
;;  8.     1: SUBSETS returned (NIL (3) (2) (2 3))
;;  9.   0: SUBSETS returned (NIL (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; 10. (NIL (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; 
;; Line 1 is the initial call.
;; In lines 2 - 4, we see successive calls to subsets generated to evaluate
;; the value of 'rest' in the procedure.  With each successive call, the
;; procedure only passes the cdr of the parameter s, so each recursive call
;; gets a shorter list passed in.  Eventually, the cdr is nil (line 5), so
;; subsets returns (nil) (line 6), which is equal to nil. 
;;
;; Next, in lines 7 - 9 we see the results from the map call on the rest list.
;; The map call iterates the elements of rest and conses on car that was 
;; stripped off in recursive calls to subsets.  This has the result of adding
;; the value to each of the returned values.  In addition, the unmodified value
;; of rest is appended to the result.  This extends the result list to include
;; new elements, so the result includes the unmodified rest plus a version of 
;; rest that has the car values prepended. Finally the new result is returned
;; to become the new rest for the previous layer of recursion (if any.)
;; 
;; Ultimately, they result list gets built up from the right to left as new
;; elements are appended into the result.
