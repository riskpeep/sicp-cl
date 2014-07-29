;Exercise 1.41:
;
;  Define a procedure double that takes a procedure of one argument as 
;  argument and returns a procedure that applies the original procedure twice.
;  For example, if inc is a procedure that adds 1 to its argument, then
;  (double inc) should be a procedure that adds 2.
;
;  What value is returned by
;
;  (((double (double double)) inc) 5)

;; We begin by writing double
;; Note, we use my-double here to avoid a name collision with the SB-ALIEN
;; package
(defun my-double (funx)
  (lambda (x) (funcall funx (funcall funx x))))

;; To test we'll use the CL 1+ function, which takes a single parameter and
;; returns that parameter incremented by 1
(= 3 (1+ 2))

;; Now we pass that same method into double to return a function that 
;; increments by two.
(= 4 (funcall (my-double #'1+) 2))

;; ;  What value is returned by
;; ;
;; ;  (((double (double double)) inc) 5)
;;
;; In this call, we pass double into itself, thus returning a procedure that
;; applies double twice.  We then pass that procedure to double again,
;; yielding a procedure that applies double 4 times.  Since each time we apply
;; double, we apply the passed procedure twice, we would expect that passing
;; inc would increment 5 by 2 * 2 * 2 * 2, or 16 times.  5 + 16 = 21.
;; 
;; We can confirm this by evaluating the given form and checking the answer.
(= 21 (funcall (funcall (my-double (my-double #'my-double)) #'1+) 5))

