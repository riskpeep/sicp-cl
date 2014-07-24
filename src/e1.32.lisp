;Exercise 1.32:
;
;  a. Show that sum and product (Exercise 1.31) are both special cases of a 
;  still more general notion called accumulate that combines a collection of 
;  terms, using some general accumulation function:
;
;  (accumulate combiner null-value term a next b)
;
;  Accumulate takes as arguments the same term and range specifications as sum
;  and product, together with a combiner procedure (of two arguments) that
;  specifies how the current term is to be combined with the accumulation of
;  the preceding terms and a null-value that specifies what base value to use
;  when the terms run out. Write accumulate and show how sum and product can
;  both be defined as simple calls to accumulate.
;
;  b. If your accumulate procedure generates a recursive process, write one
;  that generates an iterative process.  If it generates an iterative process,
;  write one that generates a recursive process.


;; From e1.30 & e1.31, we have
;; 
;; (defun sum (term a next b)
;;   (defun iter (a result)
;;     (if (> a b)
;;       result
;;       (iter (funcall next a) (+ result (funcall term a)))))
;;   (iter a 0) 
;;
;; (defun product (term a next b)
;;   (defun iter (a result)
;;     (if (> a b)
;;       result
;;       (iter (funcall next a) (* result (funcall term a)))))
;;   (iter a 1))
;;
;; Given these implementations, we can implement accumulate as follows 
(defun accumulate (combiner null-value term a next b)
  (labels ((iter (a result)
    (if (> a b)
      result
      (iter (funcall next a) (funcall combiner result (funcall term a))))))
  (iter a null-value)))

;; Using accumulate, we can implement sum and product
(defun sum (term a next b)
  (accumulate #'+ 0 term a next b))

(defun product (term a next b)
  (accumulate #'* 1 term a next b))

;; Now we define some utility functions so we can test our implementations
(defun inc (n) (+ n 1))
;; (defun identity (x) x)   ;; CL provides identity
;; 
;; First we test sum
(defun sum-series (x) 
  (sum #'identity 1 #'inc x))

;; And some tests to show that it works
(= 1 (sum-series 1))
(= 3 (sum-series 2))
(= 6 (sum-series 3))
(= 10 (sum-series 4))

;; Next we test product
(defun factorial (x) 
  (product #'identity 1 #'inc x))

;; And some tests to show that it works
(= 1 (factorial 1))
(= 2 (factorial 2))
(= 6 (factorial 3))
(= 24 (factorial 4))

;;
;;  Now for part b.
;;  
;;  b. If your accumulate procedure generates a recursive process, write one
;;  that generates an iterative process.  If it generates an iterative process,
;;  write one that generates a recursive process.
;;
;;  The accumulate procedure shown above generates an iterative process.  We can 
;;  modify it to produce a recursive process as shown below
;;  
(defun recursive-accumulate (combiner null-value term a next b)
   (labels ((iter (a)
     (if (> a b)
       null-value
       (funcall combiner (funcall term a) (iter (funcall next a))))))
   (iter a))) 

;; Finally, using our recursive-product function we can define factorial as follows
(defun recursive-product (term a next b)
  (recursive-accumulate #'* 1 term a next b))

(defun recursive-factorial (x) 
  (recursive-product #'identity 1 #'inc x))

;; And some tests to show that it works
(= 1 (recursive-factorial 1))
(= 2 (recursive-factorial 2))
(= 6 (recursive-factorial 3))
(= 24 (recursive-factorial 4))

