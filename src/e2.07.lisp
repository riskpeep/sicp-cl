;Exercise 2.7: 
;
;  Alyssa’s program is incomplete because she has not specified the 
;  implementation of the interval abstraction.  Here is a definition of the 
;  interval constructor:
;
;  (define (make-interval a b) (cons a b))
;
;  Define selectors upper-bound and lower-bound to complete the implementation.

;; First, we write make-interval in CL
(defun make-interval (a b) (cons a b))

;; Next we can write selectors for upper-bound and lower-bound
(defun lower-bound (a) (cdr a))
(defun upper-bound (a) (car a))
