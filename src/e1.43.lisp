;Exercise 1.43:
;
;  If f is a numerical function and n is a positive integer, then we can form 
;  the nth repeated application of f , which is defined to be the function 
;  whose value at x is f(f(...(f (x))...)). For example, if f is the function
;  x → x + 1, then the nth repeated application of f is the function
;  x → x + n. If f is the operation of squaring a number, then the nth 
;  repeated application of f is the function that raises its argument to the 
;  2n-th power. Write a procedure that takes as inputs a procedure that 
;  computes f and a positive integer n and returns the procedure that computes
;  the nth repeated application of f . Your procedure should be able to be 
;  used as follows:
;
;  ((repeated square 2) 5)
;  625
;
;  Hint: You may find it convenient to use compose from Exercise 1.42.

;; From e1.41, we have compose
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;; We can define our repeated procedure as follows
(defun repeated (f n)
  (if (= n 1)
    f
    (compose f (repeated f (- n 1)))))

;; And we can test it using 1+ and square
(= 6 (funcall (repeated #'1+ 5) 1))

