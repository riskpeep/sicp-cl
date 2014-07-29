;Exercise 1.42:
;
;  Let f and g be two one-argument functions.  The composition f after g is
;  defined to be the function x -> f(g(x)). Define a procedure compose that 
;  implements composition.  For example, if inc is a procedure that adds 1 to
;  its argument,
;
;  ((compose square inc) 6)

;; In order to closely follow the question we will define square, and use the 
;; CL 1+ function
(defun square (x)
  (* x x))

;; Next we write compose
(defun compose (f g)
  (lambda (x) (funcall f (funcall g x))))

;; We can test our answer using the given example.
;; We would expect that the returned value is 
;; (square (inc 6)) 
;; (square 7)
;; 49
(= 49 (funcall (compose #'square #'1+) 6))
