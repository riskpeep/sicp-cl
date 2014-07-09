;Exercise 1.4:
;
;  Observe that our model of evaluation allows for combinations whose
;  operators are compound expressions.  Use this observation to describe
;  the behavior of the following procedure:
;
;  (define (a-plus-abs-b a b)
;    ((if (> b 0) + -) a b))

;;  This method adds the absolute value of b to the value a
;;  It works by evaluating a function of b (is b greater than zero) to 
;;  determine the operator to use on the operands a and b.  If b is 
;;  greater than zero, add the two numbers, if b is less than zero, 
;;  subtract them.  This has the effect of taking the absolute value of b
;;  since subtracting a negative number is the same as adding its postive
;;  value.
;;
;;  Re-writing in lisp yields
(defun a-plus-abs-b (a b)
  (funcall ( if (> b 0) #'+ #'-) a b ))

;; Here we see one of the first substantive differences between Scheme and
;; Common Lisp.  In Scheme, it is possible to return a function and call it
;; directly as the book example shows.  In CL we must use the 'funcall'
;; function to call the returned function.  In addition, we must use the #'
;; notation to indicate that we are referring to a function when defining the
;; if statement.  Both of these issues arise from the fact that CL has
;; separate namespaces for variables and function names.  The extra syntax is
;; what tells the compiler that we're referring to a function and not a
;; variable

;;  We can confirm that we have identified a correct solution by showing the
;;  equality of two evaluations with similar operands where a'= a and b' = -b
(= (a-plus-abs-b 5 7) (a-plus-abs-b 5 -7))
