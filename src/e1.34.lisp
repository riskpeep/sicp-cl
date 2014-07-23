;Exercise 1.34:
;
;  Suppose we define the procedure
;  (define (f g) (g 2))
;
;  Then we have
;
;  (f square)
;  4
;
;  (f (lambda (z) (* z (+ z 1))))
;  6
;
;  What happens if we (perversely) ask the interpreter to evaluate the 
;  combination (f f)? Explain.
;

;; We can answer this question using substitution.
;; 
;; (f f)
;; (f (f 2))
;; (f (f (2 2)))
;; *ERR*
;; 
;; Ultimately this procedure results in an attempt to evaluate 2 as a function
;; and there is no function with the name 2 defined, so the procedure will
;; generate an error.
;; 
;; We can demonstrate that this understanding is correct by defining and 
;; evaluating f as described.
(defun f (g) (funcall g 2))

;; Here CL requires that we use the #' syntax to pass f as a function into the
;; call to f
(f #'f)

;; Executing this form causes SBCL to emit the following error
;; 
;;   The value 2 is not of type (OR FUNCTION SYMBOL).
;;   [Condition of type TYPE-ERROR]
