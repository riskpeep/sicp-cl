;Exercise 1.31:
;
;  a. The sum procedure is only the simplest of a vast number of similar 
;  abstractions that can be captured as higher-order procedures.  Write an 
;  analogous procedure called product that returns the product of the values 
;  of a function at points over a given range. Show how to define factorial in
;  terms of product. Also use product to compute approximations to ∏ using the
;  formula:
;
;   ∏     2 * 4 * 4 * 6 * 6 * 8 ...
;  --- = ---------------------------
;   4     3 * 3 * 5 * 5 * 7 * 7 ...
;
;  b. If your product procedure generates a recursive process, write one that 
;  generates an iterative process. If it generates an iterative process, write 
;  one that generates a recursive process.
;

;; From e1.30, we have
;; 
;; (defun sum (term a next b)
;;   (defun iter (a result)
;;     (if (> a b)
;;       result
;;       (iter (funcall next a) (+ result (funcall term a)))))
;;   (iter a 0) 
;;
;; Using e1.30 as an example we can write our product function
(defun product (term a next b)
   (defun iter (a result)
     (if (> a b)
       result
       (iter (funcall next a) (* result (funcall term a)))))
   (iter a 1))

;; Next we'll define an incrementer
(defun inc (n) (+ n 1))

;; And using our product function we can define factorial as follows
;; First we need another utility function for the term.  Here we use the 
;; identity function.  CL defines an identity function as part of the 
;; language, so we'll use it, but if it did not, its definition would proceed
;; as follows
;; 
;; (defun identity (x) x) 

;; Finally, using our product function we can define factorial as follows
(defun factorial (x) 
  (product #'identity 1 #'inc x))

;; And some tests to show that it works
(= 1 (factorial 1))
(= 2 (factorial 2))
(= 3 (factorial 3))

;; For computing ∏ the problem gives:
;;
;;   ∏     2 * 4 * 4 * 6 * 6 * 8 ...
;;  --- = ---------------------------
;;   4     3 * 3 * 5 * 5 * 7 * 7 ...
;;
;; We can re-write this as follows to solve for ∏
;;
;;            2 * 4 * 4 * 6 * 6 * 8 ...
;;   ∏ = 4 * ---------------------------
;;            3 * 3 * 5 * 5 * 7 * 7 ...
;;
;; With equation we can write a function to compute ∏
;;
;;
(defun next-pi-numerator (x)
  (cond ((evenp x) 
         (+ x 2.0))
        (t (+ (1- x) 2.0))))

(defun next-pi-denominator (x)
  (cond ((evenp x) 
         (+ (1- x) 2))
        (t (+ x 2))))


(defun calc-pi (steps)
  (* 4
     (/ (product #'next-pi-numerator 1 #'inc steps)
        (product #'next-pi-denominator 1 #'inc steps))))


